# Python Execution Model

## Structure of a program

Constructed from code _blocks_: a module, function body, class defintion, a script file, a script command (`python -c`).

A code block is executed in an _execution frame_, containing some administrative information (used for debugging)

## Name and Binding

Names refer to objects. Names are introduced by name binding operations.

A target occurring in a del statement is also considered bound for this purpose (though the actual semantics are to unbind the name).

### Resolution

A scope defines the visibility of a name within a block. If a local variable is defined in a block, its scope includes that block. If the definition occurs in a function block, the scope extends to any blocks contained within the defining one, unless a contained block introduces a different binding for the name.

When a name is used in a code block, it is resolved using the nearest enclosing scope. The set of all such scopes visible to a code block is called the block’s environment.

If a name binding operation occurs anywhere within a code block, all uses of the name within the block are treated as references to the current block. 

`global`: all uses of the name specified in the statement refer to the binding of that name in the top-level namespace. Names are resolved in the top-level namespace by searching the global namespace, i.e. the namespace of the module containing the code block, and the builtins namespace, the namespace of the module builtins.

`nonlocal`: causes corresponding names to refer to previously bound variables in the nearest enclosing function scope.
