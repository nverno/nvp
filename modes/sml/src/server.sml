structure Server =
struct 

fun serve port = 
    let 
        fun run listener = 
            let 
                fun accept() = 
                    let 
                        val(conn, conn_addr) = Socket.accept listener
                    in 
                        respond conn;
                        accept()
                    end
                        
                and respond conn = 
                    let
                        val msg = "hello from tcpserver\n"
                        val buf = Word8VectorSlice.full(Byte.stringToBytes msg);
                    in
                        ignore(Socket.sendVec(conn, buf));
                        Socket.close conn
                    end
                    handle x => (Socket.close conn; raise x) (* Word8Vector.vector *)
                                   
            in
                Socket.Ctl.setREUSEADDR(listener, true);
                Socket.bind(listener, INetSock.any port);
                Socket.listen(listener, 9);
                accept()
            end
            handle x => (Socket.close listener; raise x)
    in 
        run(INetSock.TCP.socket())
    end
    handle OS.SysErr(msg, _) => raise Fail(msg ^ "\n")

fun main (prog_name, args) =
    let 
        val _ = print ("Program name: " ^ prog_name ^ "\n")
                      (* val SOME port = Int.fromString(List.nth(args, 0)) *)
    in
        serve 8080
    end

end
