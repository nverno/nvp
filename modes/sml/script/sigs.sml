fun boundSignatures () =
    let
        fun isSignature symbol = Symbol.nameSpace symbol = Symbol.SIGspace
        val signatures = List.filter isSignature (EnvRef.listBoundSymbols())
    in List.app (fn s => print (Symbol.name s ^ "\n")) signatures
    end

fun boundSymbol f =
    let
        val syms = List.filter f (EnvRef.listBoundSymbols())
    in List.app (fn s => print (Symbol.name s ^ "\n")) syms
    end

val values = (fn sym => Symbol.nameSpace sym = Symbol.VALspace);
(* 
 * Force autoloading to get bindings in top-level environment
 * CM.make("$/basis.cm") 
 *)
