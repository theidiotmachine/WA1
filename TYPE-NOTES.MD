# Type notes

## Union types

### Internal union types

Each type has a const member which is a discriminator. Means 
1. a vtable so I can call a function to get the member - or maybe an offset. Probably that
1. which means a box!

### External union types

Each type is boxed in a container which has a type + a pointer? Can I get away with a memory wrapper rather than a box? 

### Magic union type

There is an external function that gives you the type. It's not on the vtable, because it doesn't have a vtable. 

### Unions of unions

* Internal and external becomes an external
* Internal and magic is hard. Probably means boxing the magic
* External and magic means boxing the magic

So do I want internal? Maybe not actually. The great thing about external is that you only pay the price if you union. The great thing about 
internal is there isn't a box. Except... there is. Because how do you get at the member?

## type types

Are basically 
1. an alias
1. a set of functions associated with the type. 


## Switch

Switch is a branch table for values 0 - n, where n is the last contiguous value? after than it is nested ifs.

## interfaces

Have a vtable but classes don't? That means boxing interfaces?

## type classes

compil
