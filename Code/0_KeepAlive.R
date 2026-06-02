# Run as a background job: tools → Jobs → Start Local Job → paste this
while(TRUE) {
  Sys.sleep(450) #seconds
  message("keepalive: ", Sys.time())
}
