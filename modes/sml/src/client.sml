(* http://mlton.org/References.attachments/Shipman02.pdf *)
structure Client = struct

 fun connect port =
   let 
     val localhost = valOf(NetHostDB.fromString "127.0.0.1")
     val addr = INetSock.toAddr(localhost, port)
     val sock = INetSock.TCP.socket()
                                      
     fun call sock =
       let 
         val _ = Socket.connect(sock, addr)
         val msg = Socket.recvVec(sock, 1000)
         val text = Byte.bytesToString msg
       in
         print text;
         Socket.close sock
       end
       handle x => (Socket.close sock; raise x)
   in
     call sock
   end
   handle OS.SysErr(msg, _) => raise Fail(msg ^ "\n")

 fun main (prog_name, args) = 
   let 
     val _ = connect 8080
   in 
     0
   end
end
