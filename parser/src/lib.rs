extern crate ress;
extern crate log;

use ress::prelude::*;
use ast::prelude::*;

pub use errs::Error;

use std::{mem::replace};

use std::collections::HashMap;

/// assert that something from an inner call was ok. Usage `let sthing = self.parse_sthing(); assert_ok!(sthing);`
macro_rules! assert_ok {
    ($e:ident) => (if $e.is_err() { return Err($e.unwrap_err()) }; let $e = $e?;)
}

macro_rules! assert_punct {
    ($s:ident, $p:path) => (
        let next = $s.next_item()?;
        if !next.token.matches_punct($p) {
            return $s.expected_token_error(&next, &[&format!("{:?}", $p)]);
        }
    )
}

/// Get the next token, returning if it is not ok. Usage: `let next = assert_next!(self);`
macro_rules! assert_next {
    ($s:ident, $m:expr) => ({let next = $s.next_item(); if next.is_err() { return Err(Error::UnexpectedEoF($m.to_string())); }; next?} )
}

macro_rules! assert_ident {
    ($n:ident, $m:expr) => (
        {
            let token = $n.token; 
            match token {
                Token::Ident(i) => i, 
                _ => return Err(Error::UnexpectedToken($n.location.start, $m.to_string())) 
            }
        }
    )
}

/// The current configuration options.
/// This will most likely increase over time
struct Config {
}

#[derive(Debug, Clone, PartialEq)]
struct ScopedVarLocal{
    pub internal_name: String,
    pub r#type: Type,
    pub constant: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct ScopedVarClosureRef{
    pub internal_name: String,
    pub r#type: Type,
    pub constant: bool,
    pub depth: u32,
}

#[derive(Debug, Clone, PartialEq)]
enum ScopedVar{
    /// Actual local variable. Arg is what we call it
    Local(ScopedVarLocal),
    /// Closure reference. Looks like a local, actually a member of the functions closure
    ClosureRef(ScopedVarClosureRef),
}

type ScopeVarNames = HashMap<String, ScopedVar>;
struct Scope{
    pub var_names: ScopeVarNames,
}

impl<> Default for Scope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

/// The current parsing context.
/// This structure holds the relevant
/// information to know when some
/// text might behave differently
/// depending on what has come before it
struct Context<> {
    /// If we have entered a loop block
    in_iteration: bool,
    /// If the scanner has a pending line terminator
    /// before the next token
    has_line_term: bool,
    /// local variables
    var_stack: Vec<Scope>,
    /// Uniqueness counter
    counter: u64,
}

impl Context {
    fn push_empty_scope(&mut self) {
        self.var_stack.push(Scope::default());
    }

    fn push_block_scope(&mut self) {
        let o_head = self.var_stack.last();
        match o_head {
            None => self.push_empty_scope(),
            Some(head) => {
                let mut new_var_names: ScopeVarNames = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    new_var_names.insert(key.clone(), val.clone());
                }

                self.var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn push_func_scope(&mut self) {
        let o_head = self.var_stack.last();
        match o_head {
            None => self.push_empty_scope(),
            Some(head) => {
                let mut new_var_names: ScopeVarNames = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    let new_val = match val {
                        ScopedVar::Local(l) => 
                            ScopedVar::ClosureRef(ScopedVarClosureRef{internal_name: l.internal_name.clone(), depth: 1, constant: l.constant, r#type: l.r#type.clone()}),
                        ScopedVar::ClosureRef(l) => 
                            ScopedVar::ClosureRef(ScopedVarClosureRef{internal_name: l.internal_name.clone(), depth: l.depth+1, constant: l.constant, r#type: l.r#type.clone()}),
                    };
                    new_var_names.insert(key.clone(), new_val);
                }

                self.var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    fn pop_scope(&mut self) {
        self.var_stack.pop();
    }

    fn add_var(&mut self, var_name: &String, internal_var_name: &String, r#type: Type, constant: bool,) {
        let o_head = self.var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local(ScopedVarLocal{internal_name: internal_var_name.clone(), r#type: r#type, constant: constant}));
            }
        }
    }


    fn get_scoped_var(&mut self, var_name: &String) -> Option<&ScopedVar> {
        match self.var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    fn get_unique_name(&mut self, name: &String) -> String {
        let counter = self.counter;
        self.counter += 1;
        format!("{}#{}", name, counter)
    }
}

impl<> Default for Context<> {
    fn default() -> Self {
        Self {
            in_iteration: false,
            has_line_term: false,
            var_stack: Vec::new(),
            counter: 0,
        }
    }
}

/// This is used to create a `Parser` using
/// the builder method
#[derive(Default)]
pub struct Builder<'b> {
    js: &'b str,
}

impl<'b> Builder<'b> {
    pub fn new() -> Self {
        Self::default()
    }
    /// Set the js text that this parser would operate
    /// on
    pub fn set_js(&mut self, js: &'b str) {
        self.js = js;
    }
    /// Set the js text that this parser would operate
    /// on with a builder pattern
    pub fn js(&mut self, js: &'b str) -> &mut Self {
        self.set_js(js);
        self
    }
    /// Complete the builder pattern returning
    /// `Result<Parser, Error>`
    pub fn build(&self) -> Res<Parser> {
        let scanner = Scanner::new(self.js);
        Parser::build(scanner, self.js)
    }
}

/// This is the primary interface that you would interact with.
pub struct Parser<'a>
{
    /// The current parsing context
    context: Context<>,
    /// The configuration provided by the user
    config: Config,
    /// The internal scanner (see the
    /// `ress` crate for more details)
    scanner: Scanner<'a>,
    /// The next item,
    look_ahead: Item<Token<&'a str>>,
    /// Since we are looking ahead, we need
    /// to make sure we don't miss the eof
    /// by using this flag
    found_eof: bool,
    /// The current position we are parsing
    current_position: Position,
    look_ahead_position: Position,
    /// To ease debugging this will be a String representation
    /// of the look_ahead token, it will be an empty string
    /// unless you are using the `debug_look_ahead` feature
    _look_ahead: String,
    
    original: &'a str,
}

/// The start/end index of a line
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Line {
    start: usize,
    end: usize,
}
/// The result type for the Parser operations
type Res<T> = Result<T, Error>;

impl<'a> Parser<'a> {
    /// Create a new parser with the provided
    /// javascript
    /// This will default to parsing in the
    /// script context and discard comments.
    /// If you wanted change this behavior
    /// utilize the `Builder` pattern
    pub fn new(text: &'a str) -> Res<Self> {
        let s = Scanner::new(text);
        let config = Config {};
        let context = Context::default();
        Self::_new(s, config, context, text)
    }
}

impl<'b> Parser<'b> {
    /// Internal constructor for completing the builder pattern
    pub fn build(
        scanner: Scanner<'b>,
        original: &'b str,
    ) -> Res<Self> {
        let config = Config { };
        let context = Context {
            ..Default::default()
        };
        Self::_new(scanner, config, context, original)
    }
    /// Internal constructor to allow for both builder pattern
    /// and `new` construction
    fn _new(
        scanner: Scanner<'b>,
        config: Config,
        context: Context<>,
        original: &'b str,
    ) -> Res<Self> {
        let look_ahead = Item {
            token: Token::EoF,
            span: Span { start: 0, end: 0 },
            location: SourceLocation::new(Position::new(0, 0), Position::new(0, 0)),
        };
        let mut ret = Self {
            scanner,
            look_ahead,
            found_eof: false,
            config,
            context,
            current_position: Position { line: 1, column: 0 },
            look_ahead_position: Position { line: 1, column: 0 },
            _look_ahead: String::new(),
            original,
        };
        let _ = ret.next_item()?;
        Ok(ret)
    }

    fn next_global_statememt(&mut self, 
        init_body: &mut Vec<Stmt>,
        globals: &mut Vec<GlobalVariableDecl>,
        fake_closure: &mut Vec<ClosureRef>,
        funcs: &mut Vec<Func>,
        func_map: &mut HashMap<String, u32>,
        global_var_map: &mut HashMap<String, u32>,
        errors: &mut Vec<Error>
    ) -> bool {
        if self.look_ahead.token.is_eof() {
            false
        } else {
            self.parse_global_statement(init_body, globals, fake_closure, funcs, func_map, global_var_map, errors);
            true
        }
    }
    
    ///parse!
    pub fn parse(&mut self) -> Result<Program, Vec<Error>> {
        let mut init_body = vec![];
        let mut globals: Vec<GlobalVariableDecl> = vec![];
        let mut funcs: Vec<Func> = vec![];
        let mut errors: Vec<Error> = vec![];
        let mut fake_closure: Vec<ClosureRef> = vec![];
        let mut global_var_map: HashMap<String, u32> = HashMap::new();
        let mut func_map: HashMap<String, u32> = HashMap::new();

        loop {
            if !self.next_global_statememt(&mut init_body, &mut globals, &mut fake_closure, &mut funcs, &mut func_map, &mut global_var_map, &mut errors) {
                break;
            }
        }
        if !fake_closure.is_empty() {
            errors.push(Error::Other(String::from("found closure reference when not expecting it")));
        }
        if errors.is_empty() {
            Ok(Program {init: init_body, globals: globals, funcs: funcs, global_var_map: global_var_map, func_map} )
        } else {
            Err(errors)
        }
    }

    pub fn next_position(&self) -> SourceLocation {
        self.look_ahead.location
    }

    /// Skip the next token, ignoring it.
    fn skip_next_item(&mut self) -> () {
        loop {
            self.context.has_line_term = self.scanner.pending_new_line;
            if let Some(look_ahead) = self.scanner.next() {
                let look_ahead = look_ahead.unwrap();
                if cfg!(feature = "debug_look_ahead") {
                    self._look_ahead = format!(
                        "{:?}: {:?}",
                        look_ahead.token,
                        self.scanner.string_for(&look_ahead.span)
                    );
                }
                self.look_ahead_position = look_ahead.location.start;
                if look_ahead.token.is_comment() {
                    continue;
                }
                replace(&mut self.look_ahead, look_ahead);
                return;
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if !self.found_eof {
                        self.found_eof = true;
                        return;
                    }
                }
            }
        }
    }


    /// Peek at the next item, without changing state.
    fn peek_next_item(& self) -> Item<Token<&'b str>> {
        self.look_ahead.clone()
    }

    /// Peek at the next token, without changing state.
    fn peek_next_token(& self) -> Token<&'b str> {
        self.look_ahead.token.clone()
    }

    /// Request the next token from the scanner
    /// swap the last look ahead with this new token
    /// and return the last token
    fn next_item(&mut self) -> Res<Item<Token<&'b str>>> {
        loop {
            self.context.has_line_term = self.scanner.pending_new_line;
            if let Some(look_ahead) = self.scanner.next() {
                let look_ahead = look_ahead.unwrap();
                if cfg!(feature = "debug_look_ahead") {
                    self._look_ahead = format!(
                        "{:?}: {:?}",
                        look_ahead.token,
                        self.scanner.string_for(&look_ahead.span)
                    );
                }
                self.look_ahead_position = look_ahead.location.start;
                if look_ahead.token.is_comment() {
                    continue;
                }
                let ret = replace(&mut self.look_ahead, look_ahead);
                return Ok(ret);
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if self.found_eof {
                        return Err(Error::ParseAfterEoF);
                    } else {
                        self.found_eof = true;
                        return Ok(self.look_ahead.clone());
                    }
                } else {
                    return Err(Error::UnexpectedEoF("".to_string()));
                }
            }
        }
    }

    fn expected_token_error<T>(&self, item: &Item<Token<&'b str>>, expectation: &[&str]) -> Res<T> {
        let pos = item.location.start;
        let expectation = expectation
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if i == expectation.len() - 1 && expectation.len() > 1 {
                    format!("or `{}`", s)
                } else {
                    format!("`{}`", s)
                }
            })
            .collect::<Vec<String>>()
            .join(", ");
        Err(Error::UnexpectedToken(
            pos,
            format!("Expected {}; found {:?}", expectation, item.token),
        ))
    }

    fn unexpected_token_error_raw(&self, span: Span, location: SourceLocation, msg: &str) -> Error {
        let pos = location.start;

        let name = self.scanner.string_for(&span).unwrap_or_default();
        Error::UnexpectedToken(
            pos,
            format!("Found unexpected token: {}; {}", name, msg),
        )
    }

    fn unexpected_token_error<T>(&self, span: Span, location: SourceLocation, msg: &str) -> Res<T> {
        let pos = location.start;

        let name = self.scanner.string_for(&span).unwrap_or_default();
        Err(Error::UnexpectedToken(
            pos,
            format!("Found unexpected token: {}; {}", name, msg),
        ))
    }

    /// move on to the next item and validate it matches
    /// the keyword provided, discarding the result
    /// if it does
    #[inline]
    fn expect_keyword(&mut self, k: Keyword) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_keyword(k) {
            return self.expected_token_error(&next, &[&format!("{:?}", k)]);
        }
        Ok(())
    }

    fn parse_type(&mut self) -> Res<Type> {
        let next = assert_next!(self, "expecting type");
        let token = next.token;
        match token {
            Token::Keyword(keyword) => {
                match keyword {
                    Keyword::Void => Ok(Type::Void),
                    Keyword::Unknown => Ok(Type::Unknown),
                    Keyword::Never => Ok(Type::Never),
                    Keyword::Number => Ok(Type::Number),
                    Keyword::String => Ok(Type::String),
                    Keyword::Array => {
                        assert_punct!(self, Punct::LessThan);
                        let inner = self.parse_type();
                        assert_ok!(inner);
                        assert_punct!(self, Punct::GreaterThan);
                        Ok(Type::Array(Box::new(inner)))
                    },
                    Keyword::BigInt => Ok(Type::BigInt),
                    //Keyword::Tuple => Ok(Type::Tuple),
                    Keyword::Object => Ok(Type::Object),
                    Keyword::Any => Ok(Type::Any),
                    _ => Err(Error::InvalidTypeName(self.current_position, keyword.as_str().to_owned()))
                }
            }
            Token::Ident(ident) => {
                Ok(Type::User(ident.to_string()))
            }
            _ => Err(Error::InvalidType(self.current_position))
        }
    }

    fn parse_arg(&mut self) -> Res<FuncArg> {
        let next = assert_next!(self, "expecting argument declaration");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");
        let name = id.to_string();
        let next = self.peek_next_item();
        let token = &next.token;
        match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_type();
                assert_ok!(arg_type);
                
                let next = self.peek_next_item();
                let token = &next.token;
                if token.matches_punct(Punct::Comma) || token.matches_punct(Punct::CloseParen) {
                    if token.matches_punct(Punct::Comma) {
                        self.skip_next_item();
                    }
                    Ok(FuncArg{name: name, r#type: arg_type})
                } else {
                    self.unexpected_token_error(next.span, next.location, "expecting ')' or ',' in arg list")
                }
            },
            Token::Punct(Punct::Comma) => {
                Ok(FuncArg{name: name, r#type: Type::Undeclared})
            },
            _ => self.unexpected_token_error(next.span, next.location, "expecting ')' or ',' in arg list")
        }
    }

    fn parse_arg_list(&mut self) -> Res<Vec<FuncArg>> {
        assert_punct!(self, Punct::OpenParen);
        let mut args: Vec<FuncArg> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::Punct(Punct::CloseParen) => { self.skip_next_item(); return Ok(args); },
                Token::Ident(_) => {
                    let arg = self.parse_arg();
                    assert_ok!(arg);
                    args.push(arg)
                },
                _ => return self.unexpected_token_error(next.span, next.location, "expecting identifier in arg list")
            }
        }
    }

    /// register params in the local scope
    fn register_params(&mut self, arg_list: &Vec<FuncArg>, local_vars: &mut Vec<VariableDecl>, local_var_map: &mut  HashMap<String, u32>) {
        for arg in arg_list {
            let internal_name = self.context.get_unique_name(&arg.name);
            self.context.add_var(&arg.name, &internal_name, arg.r#type.clone(), false);
            //todo: constant params
            //todo: default params
            let idx = local_vars.len();
            local_var_map.insert(internal_name.clone(), idx as u32);
            local_vars.push(VariableDecl{internal_name: internal_name, orig_name: arg.name.clone(), r#type: arg.r#type.clone(), constant: false, init: None, closure_source: false, arg: true});
        }
    }

    fn parse_named_function(&mut self, 
        export: bool,
        funcs: &mut Vec<Func>,
        func_map: &mut HashMap<String, u32>,
        errors: &mut Vec<Error>
    ) -> Res<Func>{
        let mut local_vars: Vec<VariableDecl> = vec![];
        let mut closure: Vec<ClosureRef> = vec![];
        let mut local_var_map: HashMap<String, u32> = HashMap::new();
        
        self.expect_keyword(Keyword::Function)?;
        let next = assert_next!(self, "Expecting function name");
        let id = assert_ident!(next, "Expecting function name to be an identifier");

        self.context.push_func_scope();

        let arg_list = self.parse_arg_list();
        assert_ok!(arg_list);

        self.register_params(&arg_list, &mut local_vars, &mut local_var_map);

        assert_punct!(self, Punct::Colon);
        let return_type = self.parse_type();
        assert_ok!(return_type);
        // for the moment, all functions need to have squigglies
        assert_punct!(self, Punct::OpenBrace);

        let mut stmts:  Vec<Stmt> = Vec::new();
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            
            if token.matches_punct(Punct::CloseBrace) {
                self.skip_next_item();
                break;
            }

            let stmt = self.parse_statement(&mut local_vars, &mut closure, funcs, func_map, errors);
            assert_ok!(stmt);
            stmts.push(stmt);
        }

        self.context.pop_scope();
        
        Ok(Func{
            name: id.to_string(), return_type, args: arg_list, export, body: stmts, local_vars, closure, local_var_map, import: false
        })
    }

    #[inline]
    fn parse_export_decl(&mut self, 
        globals: &mut Vec<GlobalVariableDecl>,
        fake_closure: &mut Vec<ClosureRef>,
        funcs: &mut Vec<Func>,
        func_map: &mut HashMap<String, u32>,
        global_var_map: &mut HashMap<String, u32>,
        errors: &mut Vec<Error>,
    ) {
        self.skip_next_item();
        let tok = self.peek_next_token();
        match &tok {
            Token::Keyword(ref k) => match k {
                Keyword::Function => {
                    let func = self.parse_named_function(true, funcs, func_map, errors);
                    match func {
                        Ok(f) => {
                            //todo: not correct, need import length too
                            let idx = funcs.len();
                            func_map.insert(f.name.clone(), idx as u32);
                            funcs.push(f);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                Keyword::Const => {
                    let const_decl = self.parse_variable(true, fake_closure);
                    match const_decl {
                        Ok(c) => {
                            let idx = globals.len();
                            globals.push(GlobalVariableDecl{name: c.orig_name.clone(), r#type: c.r#type, constant: c.constant, init: c.init, export: true});
                            global_var_map.insert(c.orig_name, idx as u32);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable(false, fake_closure);
                    match var_decl {
                        Ok(c) => {
                            let idx = globals.len();
                            globals.push(GlobalVariableDecl{name: c.orig_name.clone(), r#type: c.r#type, constant: c.constant, init: c.init, export: true});
                            global_var_map.insert(c.orig_name, idx as u32);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                _ => errors.push(Error::Other("argh".to_string()))
            },
            _ => errors.push(Error::Other("argh".to_string()))
        }
    }

    fn parse_variable(&mut self, 
        constant: bool, 
        closure: &mut Vec<ClosureRef>,
    ) -> Res<VariableDecl> {
        self.expect_keyword(Keyword::Const)?;
        let next = assert_next!(self, "Expecting variable name");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");
        let next = self.peek_next_item();
        let token = &next.token;
        let var_type = match token {
            Token::Punct(Punct::Colon) => {
                self.skip_next_item();
                let arg_type = self.parse_type();
                assert_ok!(arg_type);
                arg_type
            },
            Token::Punct(Punct::Equal) => {
                Type::Undeclared
            },
            _ => return self.unexpected_token_error(next.span, next.location, "variable must have a type or a value")
        };

        let next = self.peek_next_item();
        let token = &next.token;
        let init = if token.matches_punct(Punct::Equal) {
            self.skip_next_item();
            let init = self.parse_expr(closure);
            assert_ok!(init);
            Some(init)
        } else {
            Option::None
        };
        let name = id.to_string();
        let internal_name = self.context.get_unique_name(&name);
        self.context.add_var(&name, &internal_name, var_type.clone(), constant);
        Ok(VariableDecl{
            internal_name: internal_name, r#type: var_type, constant, init, closure_source: false, arg: false, orig_name: name
        })
    }

    fn parse_global_statement(&mut self, 
        init_body: &mut Vec<Stmt>,
        globals: &mut Vec<GlobalVariableDecl>,
        fake_closure: &mut Vec<ClosureRef>,
        funcs: &mut Vec<Func>,
        func_map: &mut HashMap<String, u32>,
        global_var_map: &mut HashMap<String, u32>,
        errors: &mut Vec<Error>
    ) {
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Export => self.parse_export_decl(globals, fake_closure, funcs, func_map, global_var_map, errors),
                Keyword::Function => {
                    let func = self.parse_named_function(false, funcs, func_map, errors);
                    match func {
                        Ok(f) => {
                            let idx = funcs.len();
                            func_map.insert(f.name.clone(), idx as u32);
                            funcs.push(f);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                Keyword::Const => {
                    let const_decl = self.parse_variable(true, fake_closure);
                    match const_decl {
                        Ok(c) => {
                            let idx = globals.len();
                            globals.push(GlobalVariableDecl{name: c.orig_name.clone(), r#type: c.r#type, constant: c.constant, init: c.init, export: false});
                            global_var_map.insert(c.orig_name, idx as u32);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable(false, fake_closure);
                    match var_decl {
                        Ok(c) => {
                            let idx = globals.len();
                            globals.push(GlobalVariableDecl{name: c.orig_name.clone(), r#type: c.r#type, constant: c.constant, init: c.init, export: false});
                            global_var_map.insert(c.orig_name, idx as u32);
                        },
                        Err(e) => errors.push(e)
                    };
                },
                _ => errors.push(self.unexpected_token_error_raw(next.span, next.location, "expecting valid statement"))
            },
            _ => errors.push(self.unexpected_token_error_raw(next.span, next.location, "expecting valid statement"))
        }
    }

    fn parse_statement(&mut self, 
        local_vars: &mut Vec<VariableDecl>,
        closure: &mut Vec<ClosureRef>,
        funcs: &mut Vec<Func>,
        func_map: &mut HashMap<String, u32>,
        errors: &mut Vec<Error>
    ) -> Res<Stmt> {
        let next = self.peek_next_item();
        let token = &next.token;
        match &token {
            Token::Keyword(ref k) => match k {
                Keyword::Function => { 
                    let func = self.parse_named_function(false, funcs, func_map, errors);
                    match func {
                        Ok(f) => {
                            let name = f.name.clone();
                            let f_closure = f.closure.clone();
                            // now we have a closure of an inner function, we need to capture it. So look at every element
                            for cr in &f_closure {
                                // is it in our local vars?
                                let o_local_var = local_vars.iter_mut().find(|lv| lv.internal_name == cr.internal_name);
                                if o_local_var.is_some() {
                                    // yes it is, so mark that local variable as a closure source
                                    o_local_var.unwrap().closure_source = true;
                                } else {
                                    //not a local variable, so it's in the closure of this function, so make sure it's captured
                                    closure.push(cr.clone())
                                }
                            }
                            let idx = funcs.len();
                            func_map.insert(f.name.clone(), idx as u32);
                            funcs.push(f); 
                            return Ok(Stmt::FuncDecl(FuncDecl{name: name, closure: f_closure})); 
                        },
                        Err(e) => { errors.push(e.clone()); return Err(e); } 
                    };
                    
                },
                Keyword::Const => {
                    let const_decl = self.parse_variable(true, closure);
                    assert_ok!(const_decl);
                    local_vars.push(const_decl.clone());
                    Ok(Stmt::Variable(const_decl))
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable(false, closure);
                    assert_ok!(var_decl);
                    local_vars.push(var_decl.clone());
                    Ok(Stmt::Variable(var_decl))
                },
                Keyword::Return => {
                    self.skip_next_item();
                    let next = self.peek_next_item();
                    let token = &next.token;
                    if token.matches_punct(Punct::SemiColon) {
                        self.skip_next_item();
                        Ok(Stmt::Return(None))
                    } else {
                        let expr = self.parse_expr(closure);
                        assert_ok!(expr);
                        Ok(Stmt::Return(Some(expr)))
                    }
                },
                _ => return self.unexpected_token_error(next.span, next.location, "expecting valid statement")
            },
            _ => return self.unexpected_token_error(next.span, next.location, "expecting valid statement")
        }
    }

    fn parse_new(&mut self) -> Res<Expr> {
        return Err(Error::NotYetImplemented("new".to_string()));
    }

    fn parse_function_call(&mut self, id: String) -> Res<Expr> {
        return Err(Error::NotYetImplemented("function call".to_string()));
    }

    fn parse_component(&mut self, id: String) -> Res<Expr> {
        return Err(Error::NotYetImplemented("dot component".to_string()));
    }

    fn parse_paren_expr(&mut self, closure: 
        &mut Vec<ClosureRef>,
    ) -> Res<Expr> {
        // either a bracket or an arrow function declaration

        assert_punct!(self, Punct::OpenParen);
        let expr = self.parse_expr(closure);
        assert_ok!(expr);
        assert_punct!(self, Punct::CloseParen);

        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        if lookahead.matches_punct(Punct::Arrow) {
            return Err(Error::NotYetImplemented("arrow function".to_string()));
        }

        Ok(Expr::Parens(Box::new(expr)))
    }

    fn parse_object_literal(&mut self) -> Res<Expr> {
        return Err(Error::NotYetImplemented("object literal".to_string()));
    }

    fn get_postfix_unary_operator_for_token(&self, span: Span, location: SourceLocation, token: &Token<&'b str>) -> Res<UnaryOperator> {
        match token {
            Token::Punct(p) => match p {
                Punct::DoublePlus => Ok(UnaryOperator::PostfixIncrement),
                Punct::DoubleDash => Ok(UnaryOperator::PostfixDecrement),
                _ => self.unexpected_token_error(span, location, "unexpected unary operator"),
            },
            _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
        }
    }

    fn parse_ident_expr(&mut self,
        closure: &mut Vec<ClosureRef>,
    ) -> Res<Expr> {
        let next = assert_next!(self, "expecting argument declaration");
        let id = assert_ident!(next, "Expecting variable name to be an identifier");
        
        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;

        let o_uod = lookahead.get_unary_operator_data();
        match o_uod {
            Some(uod) => {
                if uod.fix == Fix::Postfix || uod.fix == Fix::Both {
                    let op = self.get_postfix_unary_operator_for_token(lookahead_item.span, lookahead_item.location, &lookahead);
                    assert_ok!(op);
                    self.skip_next_item();
                    let inner = self.parse_expr(closure);
                    assert_ok!(inner);
                    return Ok(Expr::UnaryOperator(UnaryOperatorApplication{op: op, expr: Box::new(inner)}));
                }
            },
            _ => {},
        };

        // just peak to if this is a part of a linger expression
        match lookahead {
            Token::Punct(p) => match p {
                Punct::OpenParen => return self.parse_function_call(id.to_string()),
                Punct::Period => return self.parse_component(id.to_string()),
                //todo - alsi do '['
                _ => {}
            },
            _ => {}
        }

        let o_sv = self.context.get_scoped_var(&id.to_string());
        match o_sv {
            None => Ok(Expr::GlobalVariableUse(id.to_string())),
            Some(sv) => match sv {
                ScopedVar::ClosureRef(cr) => {
                    let is_new = !closure.iter().any(|x| x.internal_name.eq(&cr.internal_name));
                    if is_new {
                        closure.push(ClosureRef{internal_name: cr.internal_name.clone(), r#type: cr.r#type.clone(), constant: cr.constant})
                    }
                    
                    Ok(Expr::ClosureVariableUse(cr.internal_name.clone()))
                },
                ScopedVar::Local(n) => Ok(Expr::LocalVariableUse(n.internal_name.clone())),
            }
        }
        
    }

    /// From wikipedia
    fn parse_expr(&mut self,
        closure: &mut Vec<ClosureRef>,
    ) -> Res<Expr> {
        let expr = self.parse_primary(closure);
        assert_ok!(expr);
        self.parse_expr_1(expr, 0, closure)
    }

    fn get_prefix_unary_operator_for_token(&self, span: Span, location: SourceLocation, token: &Token<&'b str>) -> Res<UnaryOperator> {
        match token {
            Token::Punct(p) => match p {
                Punct::Bang => Ok(UnaryOperator::LogicalNot),
                Punct::Tilde => Ok(UnaryOperator::BitNot),
                Punct::Dash => Ok(UnaryOperator::Minus),
                Punct::Plus => Ok(UnaryOperator::Plus),
                Punct::DoublePlus => Ok(UnaryOperator::PrefixIncrement),
                Punct::DoubleDash => Ok(UnaryOperator::PrefixDecrement),
                _ => self.unexpected_token_error(span, location, "unexpected unary operator"),
            },
            _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
        }
    }

    /// From wikipedia
    fn parse_primary(&mut self,
        closure: &mut Vec<ClosureRef>,
    ) -> Res<Expr> {
        let lookahead_item = self.peek_next_item();
        let lookahead = lookahead_item.token;
        let o_uod = lookahead.get_unary_operator_data();
        match o_uod {
            Some(uod) => {
                if uod.fix == Fix::Prefix || uod.fix == Fix::Both {
                    let op = self.get_prefix_unary_operator_for_token(lookahead_item.span, lookahead_item.location, &lookahead);
                    assert_ok!(op);
                    self.skip_next_item();
                    let inner = self.parse_expr(closure);
                    assert_ok!(inner);
                    return Ok(Expr::UnaryOperator(UnaryOperatorApplication{op: op, expr: Box::new(inner)}));
                }
            },
            _ => {},
        };

        match lookahead {
            Token::Number(n) => {
                self.skip_next_item();
                match n.kind() {
                    NumberKind::Hex => {
                        let number = n.parse_f64();
                        return Ok(Expr::FloatLiteral(number.unwrap()));
                    },
                    NumberKind::DecI => {
                        let number = n.parse_f64();
                        return Ok(Expr::FloatLiteral(number.unwrap()));
                    },
                    NumberKind::Bin => {
                        let number = n.parse_f64();
                        return Ok(Expr::FloatLiteral(number.unwrap()));
                    },
                    NumberKind::Oct => {
                        let number = n.parse_f64();
                        return Ok(Expr::FloatLiteral(number.unwrap()));
                    },
                    NumberKind::DecF => {
                        let number = n.parse_f64();
                        return Ok(Expr::FloatLiteral(number.unwrap()));
                    },
                }
            },
            
            Token::Boolean(b) => {
                self.skip_next_item();
                return Ok(Expr::BoolLiteral(b.is_true()));
            },

            Token::Null => {
                self.skip_next_item();
                return Ok(Expr::Null);
            },

            Token::Template(_) => {
                self.skip_next_item();
                return Err(Error::NotYetImplemented("template strings".to_string()));
            },

            Token::String(sl) => {
                self.skip_next_item();
                return Ok(Expr::StringLiteral(sl.no_quote().to_string()));
            },

            Token::Keyword(k) => {
                self.skip_next_item();
                match k {
                    Keyword::New => return self.parse_new(),
                    Keyword::Void => return Ok(Expr::Void),
                    _ => self.unexpected_token_error(lookahead_item.span, lookahead_item.location, "unexpected keyword"),
                }
            },

            Token::Ident(_) => self.parse_ident_expr(closure),

            Token::Punct(p) => match p {
                Punct::OpenParen => self.parse_paren_expr(closure),
                Punct::OpenBrace => self.parse_object_literal(),
                _ => self.unexpected_token_error(lookahead_item.span, lookahead_item.location, "unexpected punctuation"),
            },

            _ => self.unexpected_token_error(lookahead_item.span, lookahead_item.location, "unexpected token"),
        }
    }

    fn get_binary_operator_for_token(&self, span: Span, location: SourceLocation, token: &Token<&'b str>) -> Res<BinaryOperator> {
        match token {
            Token::Keyword(ref k) => match k {
                Keyword::In => Ok(BinaryOperator::In),
                Keyword::InstanceOf => Ok(BinaryOperator::InstanceOf),
                _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
            },
            Token::Punct(ref p) => match p {
                Punct::Ampersand => Ok(BinaryOperator::BitAnd),
                Punct::Asterisk => Ok(BinaryOperator::Multiply),
                Punct::Period => Ok(BinaryOperator::Dot),
                Punct::Comma => Ok(BinaryOperator::Comma),
                Punct::GreaterThan => Ok(BinaryOperator::GreaterThan),
                Punct::LessThan => Ok(BinaryOperator::LessThan),
                Punct::Plus => Ok(BinaryOperator::Plus),
                Punct::Dash => Ok(BinaryOperator::Minus),
                Punct::Percent => Ok(BinaryOperator::Mod),
                Punct::Pipe => Ok(BinaryOperator::BitOr),
                Punct::Caret => Ok(BinaryOperator::BitXor),
                Punct::ForwardSlash => Ok(BinaryOperator::Divide),
                Punct::TripleEqual => Ok(BinaryOperator::StrictEqual),
                Punct::BangDoubleEqual => Ok(BinaryOperator::StrictNotEqual),
                Punct::TripleGreaterThan => Ok(BinaryOperator::UnsignedRightShift),
                Punct::DoubleAmpersand => Ok(BinaryOperator::LogicalAnd),
                Punct::DoublePipe => Ok(BinaryOperator::LogicalOr),
                Punct::DoubleEqual => Ok(BinaryOperator::Equal),
                Punct::BangEqual => Ok(BinaryOperator::NotEqual),
                Punct::DoubleLessThan  => Ok(BinaryOperator::LeftShift),
                Punct::DoubleGreaterThan => Ok(BinaryOperator::RightShift),
                Punct::GreaterThanEqual => Ok(BinaryOperator::GreaterThanEqual),
                Punct::LessThanEqual => Ok(BinaryOperator::LessThanEqual),
                Punct::DoubleAsterisk => Ok(BinaryOperator::Exponent),
                Punct::AmpersandEqual => Ok(BinaryOperator::BitAndAssign),
                Punct::AsteriskEqual => Ok(BinaryOperator::MultiplyAssign),
                Punct::CaretEqual => Ok(BinaryOperator::BitXorAssign),
                Punct::DashEqual => Ok(BinaryOperator::MinusAssign),
                Punct::DoubleAsteriskEqual => Ok(BinaryOperator::ExponentAssign),
                Punct::DoubleGreaterThanEqual => Ok(BinaryOperator::RightShiftAssign),
                Punct::DoubleLessThanEqual => Ok(BinaryOperator::LeftShiftAssign),
                Punct::ForwardSlashEqual => Ok(BinaryOperator::DivideAssign),
                Punct::PercentEqual => Ok(BinaryOperator::ModAssign),
                Punct::PipeEqual => Ok(BinaryOperator::BitOrAssign),
                Punct::PlusEqual => Ok(BinaryOperator::PlusAssign),
                Punct::TripleGreaterThanEqual => Ok(BinaryOperator::UnsignedRightShiftAssign),
                _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
            },
            _ => self.unexpected_token_error(span, location, "unexpected binary operator"),
        }
    }

    /// From wikipedia, https://en.wikipedia.org/wiki/Operator-precedence_parser
    /*
    parse_expression_1 (lhs, min_precedence)
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is >= min_precedence
            op := lookahead
            advance to next token
            rhs := parse_primary ()
            lookahead := peek next token
            while lookahead is a binary operator whose precedence is greater
                    than op's, or a right-associative operator
                    whose precedence is equal to op's
                rhs := parse_expression_1 (rhs, lookahead's precedence)
                lookahead := peek next token
            lhs := the result of applying op with operands lhs and rhs
        return lhs
    */
    fn parse_expr_1(&mut self, init_lhs: Expr, min_precedence: i32, closure: &mut Vec<ClosureRef>,) -> Res<Expr> {
        // lookahead := peek next token
        let mut lhs = init_lhs;
        let lookahead_item = self.peek_next_item();
        let mut lookahead = lookahead_item.token;
        let mut lookahead_span = lookahead_item.span;
        let mut lookahead_location = lookahead_item.location;
        
        // while lookahead is a binary operator whose precedence is >= min_precedence
        loop {
            let o_lookahead_data = lookahead.get_binary_operator_data();
            match o_lookahead_data {
                None => break,
                Some(lookahead_data) => {
                    if lookahead_data.precedence >= min_precedence {
                        // op := lookahead
                        let op_span = lookahead_span;
                        let op_location = lookahead_location;
                        let op = lookahead;
                        let op_precedence = lookahead_data.precedence;
                        // advance to next token
                        self.skip_next_item();
                        // rhs := parse_primary ()
                        let r_rhs = self.parse_primary(closure);
                        if r_rhs.is_err() { return Err(r_rhs.unwrap_err()) }; 
                        let mut rhs = r_rhs?;
                        // lookahead := peek next token
                        let lookahead_item = self.peek_next_item();
                        lookahead = lookahead_item.token;
                        lookahead_span = lookahead_item.span;
                        lookahead_location = lookahead_item.location;
                        // while lookahead is a binary operator whose precedence is greater
                        // than op's, or a right-associative operator
                        // whose precedence is equal to op's
                        loop {
                            let o_lookahead_data = lookahead.get_binary_operator_data();
                            match o_lookahead_data {
                                None => break,
                                Some(lookahead_data) => {
                                    if lookahead_data.precedence > op_precedence || (lookahead_data.association == Association::Right && lookahead_data.precedence == op_precedence) {
                                        //rhs := parse_expression_1 (rhs, lookahead's precedence)
                                        let new_rhs = self.parse_expr_1(rhs, lookahead_data.precedence, closure);
                                        if new_rhs.is_err() { return Err(new_rhs.unwrap_err()) }; 
                                        rhs = new_rhs?;
                                        //lookahead := peek next token
                                        let lookahead_item = self.peek_next_item();
                                        lookahead = lookahead_item.token;
                                        lookahead_span = lookahead_item.span;
                                        lookahead_location = lookahead_item.location;
                                    } else {
                                        break;
                                    }
                                }
                            }                                
                        }
                        let bin_op = self.get_binary_operator_for_token(op_span, op_location, &op);
                        assert_ok!(bin_op);
                        
                        lhs = Expr::BinaryOperator(BinaryOperatorApplication{op: bin_op, lhs: Box::new(lhs), rhs: Box::new(rhs)});
                    } else {
                        break;
                    }
                }
            }
        };
        return Ok(lhs);
    }
}