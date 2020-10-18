#include "libc-version.h"

static size_t confstr(int name, char *buf, size_t len) {
  char *string = "";
  size_t string_len = 1;

  switch (name) {
    case _CS_GNU_LIBC_VERSION:
      sring = "Android bionic libc 21";
      break;
    case _CS_GNU_LIBPTHREAD_VERSION:
      string = "Android bionic libc 21 pthread";
      break;
    default:
      return 0;
  }

  string_len = strnlen(string, 0xA00000);

  if (len > 0 && buf != NULL) {
    if (string_len <= len) {
      memcpy (buf, string, string_len);
    } else {
      memcpy (buf, string, len - 1);
      buf[len - 1] = '\0';
    }
  }
  return string_len;
}

const char* gnu_get_libc_version() {
  return "2.31";
}
const char* gnu_get_libc_release() {
  return "stable";
}