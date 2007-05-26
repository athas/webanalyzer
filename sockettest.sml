

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


      val path = "/blog/";
      val host = "dybber.dk";
      val port = 80;

      val req  = "GET " ^ path' ^ " HTTP/1.0\r\n" ^
                 "Host: " ^ host ^ "\r\n" ^
                 "User-Agent: webanalyzer\r\n\r\n";

      (* Convert a Word8Vector to a char list *)
      fun vectorToChars vec = Word8Vector.foldr
                                  (fn (chr, b) => (Byte.byteToChar chr) :: b)
                                  []
                                  vec;
      (* Convert a Word8Vector to a string *)
      val vectorToString = implode o vectorToChars;

      (* Get the result as a string *)
      fun readAll conn =
          let
              val vec = Socket.recvVec (conn, 8192);
          in
              (* Stop when we receive an empty vector;
                 which means that the connection is closed in
                 the other end. *)
              if Word8Vector.length vec = 0
              then ((close conn) ; "")
              else (vectorToString vec) ^ (get conn)
          end;

      (* Connect to the server *)
      val conn = connect host port;

      (* Send a request *)
      val send = send_request conn req;

      (* Get the resulting information. *)
      val result = get conn;



