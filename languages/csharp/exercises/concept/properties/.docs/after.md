The two main types of property are 

1. auto-implemented where the `get` and `set` accessors have no body 
2. those where the accessors evaluate expressions and execute statements.  The code can
be as simple as returning or assigning a backing field.

There is considerable overlap of behaviour and power between properties and methods.  
Although much of the time it is obvious which to use in a particular case it is
often a judgement call for the coder and in particular how much code should be
executed within the accessors.

In a similar way to other class members properties can have access levels.
Most often properties will have a non-private access level in line with
their essential purpose.  Sometimes one of the accessors will have
a different access level to the property.  In the case of `TareWeight`
under the rather artificial "security" constraint there was an opportunity
to have a public property with a private getter.