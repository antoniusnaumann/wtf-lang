> Note:
> This is a draft, none of the features mentioned here are currently implemented

# Compiler Plugins
## Setup
The WTF compiler allows custom compiler plugins to be executed during compilation of your project. By default, the compiler will look for compiler plugins in a `plugins/` directory in your project directory.

Each plugin should be located in a separate directory, that contains exactly one `.wasm` file that the compiler will load.

The following example project structure includes two plugins called `tsgenerate` and `openapi`:
```
  src/
  |- /* your WTF source files go here */
  plugins/
  |- tsgenerate/
     |- src/
        |- /* your plugin can include WTF source files */
     |- output.wasm
  |- openapi/
     |- output.wasm
```

The compiler will look for the following functions in a plugin and invoke them if they exist:

```
func register_types(types: [Type]) {}
func register_functions(funcs: [Func]) {}
```

In the future, more hooks might be added, e.g. to modify functions (e.g. to add tracing) or to add types. 
