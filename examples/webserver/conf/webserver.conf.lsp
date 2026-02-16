(webserver-config
  (listen_port 18080)
  (document_root "./examples/webserver/runtime/docroot")
  (tls_cert_file "./examples/webserver/runtime/tls/server.crt")
  (tls_key_file "./examples/webserver/runtime/tls/server.key")
  (cgi_enabled #t)
  (cgi_bin_dir "./examples/webserver/runtime/cgi-bin")
  (max_connections 100))
