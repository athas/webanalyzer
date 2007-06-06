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

      val req  = "GET " ^ path ^ " HTTP/1.1\r\n" ^
                 "Host: " ^ host ^ "\r\n" ^
                 "User-Agent: webanalyzer\r\n\r\n";

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
              else (Byte.bytesToString vec) ^ (readAll conn)
          end;

open Util;

fun getHeader socket = if readLine socket = "\r\n" then ()
                       else getHeader socket

fun readChunked conn =
    let fun readN n = let val vec = Socket.recvVec (conn, n);
                          val string = Byte.bytesToString vec;
                          val len = size string;
                      in if len > 0 andalso len < n
                         then string ^ (readN (n - len))
                         else string
                      end;
        fun readNextLine conn = let val line = readLine conn
                                in if size line > 0 andalso
                                      List.all Char.isSpace (explode line)
                                   then readNextLine conn
                                   else line
                                end;

        val chunkSizeHex = readNextLine conn
        val chunkSize = valOf (StringCvt.scanString (Int.scan StringCvt.HEX)
                                                    chunkSizeHex)
            handle _ => raise Fail ("expected hex value not : " ^ chunkSizeHex)

    in
        if chunkSize = 0
        then ""
        else let val stringResult = readN chunkSize;
             in
                 if size stringResult = 0
                 then stringResult
                 else stringResult ^ (readChunked conn)
             end
    end;


      (* Connect to the server *)
      val conn = connect host port;

      (* Send a request *)
      val send = send_request conn req;

      (* Get the resulting information. *)
      val result = (getHeader conn;
                    readChunked conn)



