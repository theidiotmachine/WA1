use serde::{Serialize, Deserialize};
use std::collections::HashSet;
use std::iter::FromIterator;

pub mod prelude {
    pub use super::TypeConstraint;
    pub use super::TypeArg;
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, Hash)]
pub enum TypeConstraint{
    ///A union of constraints. Means at least one of these are true. If a union of traits, means at least 
    /// one is present. The member functions available are the intersection of traits in the union.
    Union(Vec<TypeConstraint>),
    ///An intersection of constraints. Means every one is true. If an intersection of traits, means
    /// all are present. The member functions available are the union of the traits in the intersection.
    Intersection(Vec<TypeConstraint>),
    ///Means implements this trait
    Trait(String),
    ///No condition, so accepts everything
    None
}

///This from [https://stackoverflow.com/questions/40718975/how-to-get-every-subset-of-a-vector-in-rust]
fn generate_power_set<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
    (0..2usize.pow(s.len() as u32)).map(|i| {
         s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                             .map(|(_, element)| element.clone())
                             .collect()
     }).collect()
}   

impl TypeConstraint{
    ///whether or not this adheres to a marker trait
    pub fn contains_trait(&self, t: &String) -> bool {
        match self {
            TypeConstraint::Union(inner) => inner.iter().all(|x| x.contains_trait(t)),
            TypeConstraint::Intersection(inner) => inner.iter().any(|x| x.contains_trait(t)),
            TypeConstraint::Trait(i) => i == t,
            TypeConstraint::None => false
        }
    }

    pub fn get_all_contained_traits(&self) -> Vec<String> {
        match self {
            TypeConstraint::Union(inner) | TypeConstraint::Intersection(inner) => inner.iter().flat_map(|x| x.get_all_contained_traits()).collect(),
            TypeConstraint::Trait(i) => vec![i.clone()],
            TypeConstraint::None => vec![]
        }
    }

    pub fn conforms_to_trait_set(&self, ts: &HashSet<String>) -> bool {
        match self{
            TypeConstraint::Union(inner) => inner.iter().any(|x| x.conforms_to_trait_set(ts)),
            TypeConstraint::Intersection(inner) => inner.iter().all(|x| x.conforms_to_trait_set(ts)),
            TypeConstraint::Trait(i) => ts.contains(i),
            TypeConstraint::None => false
        }
    }

    ///Whether this is a subset of another trait. This is super expensive at the moment. I need some CS-hammer to optimise it. It basically
    /// goes through every combination of traits possible that this constraint conforms to, and checks that they are conformed to in the 
    /// other
    pub fn is_subset_of(&self, other: &TypeConstraint) -> bool {
        //first get all the contained traits of this
        let self_contained_traits = self.get_all_contained_traits();

        //now work through every combination of traits. If it's legitimate for this, it must be legitimate for the other
        let power_set = generate_power_set(&self_contained_traits);
        for s in power_set {
            let hs = HashSet::from_iter(s);
            if self.conforms_to_trait_set(&hs) {
                if !other.conforms_to_trait_set(&hs) {
                    return false;
                }
            }
        }
        true
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, Hash)]
pub struct TypeArg{
    pub name: String,
    pub constraint: TypeConstraint,
}
