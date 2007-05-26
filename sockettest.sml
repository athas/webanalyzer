
      fun connect host port =
      let
         fun resolve host =
            #addr (SockUtil.resolveAddr {host=SockUtil.HostName(host),
                                         port=NONE})
            handle BadAddr => raise Fail ("unknown host: " ^ host)
      in 
         SockUtil.connectINetStrm {addr=(resolve host), port=port}
      end

      (* Close the connection and return nothing.  If conn is already
       * closed, handle SysErr by also returning nothing.
       *)
      fun close conn =
         Socket.close conn handle _ => ()

      (* Sends a request to the remote host.  Returns the number of
       * characters sent, throwing SysErr on error.
       *)
      fun send_request conn req =
         SockUtil.sendStr (conn, req)
         handle SysErr => ( print "could not send request" ; raise SysErr )


      val path = SOME "/blog/";
      val host = "dybber.dk";
      val port = 80;

      val path' = Option.getOpt (path, "/")

      val req  = "GET " ^ path' ^ " HTTP/1.0\r\n" ^
                 "Host: " ^ host ^ "\r\n" ^
                 "User-Agent: webanalyzer\r\n\r\n";


      fun vectorToChars vec = Word8Vector.foldr
                                  (fn (chr, b) => (Byte.byteToChar chr) :: b)
                                  []
                                  vec;

      val vectorToString = implode o vectorToChars;

      fun get conn =
          let
              val vec = Socket.recvVec (conn, 10);
          in
              if Word8Vector.length vec = 0
              then ((close conn) ; "")
              else
                  (vectorToString vec) ^ (get conn)
          end;


      val conn = connect host port;
      val send = send_request conn req;
      val result = get conn;



