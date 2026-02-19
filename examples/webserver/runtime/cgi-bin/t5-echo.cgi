#!/bin/sh
echo "Status: 200 OK"
echo "Content-Type: text/plain"
echo
printf "method=%s\n" "$REQUEST_METHOD"
printf "query=%s\n" "$QUERY_STRING"
printf "len=%s\n" "$CONTENT_LENGTH"
printf "ctype=%s\n" "$CONTENT_TYPE"
printf "script=%s\n" "$SCRIPT_NAME"
printf "path_info=%s\n" "$PATH_INFO"
body=$(cat)
printf "body=%s\n" "$body"

