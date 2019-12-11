# Prototype-based programing

> From Wikipedia. Needs rewriting:

Prototype-based programming is a style of object-oriented programming in which behaviour reuse (known as inheritance) is performed via a process of reusing existing objects via delegation that serve as prototypes.
This model can also be known as prototypal, prototype-oriented, classless, or instance-based programming.
Delegation is the language feature that supports prototype-based programming.

Prototype-based programming uses generalized objects, which can then be cloned and extended.
Using fruit as an example, a "fruit" object would represent the properties and functionality of fruit in general.
A "banana" object would be cloned from the "fruit" object and general properties specific to bananas would be appended.
Each individual "banana" object would be cloned from the generic "banana" object.
Compare to the class-based paradigm, where a "fruit" class would be extended by a "banana" class.
