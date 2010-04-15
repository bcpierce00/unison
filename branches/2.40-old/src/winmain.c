#include <caml/callback.h>
#include <windows.h>

extern char **__argv;

int WINAPI WinMain(HINSTANCE h, HINSTANCE hPrevInstance, 
                   LPSTR lpCmdLine, int nCmdShow) {
  caml_main(__argv);
  return 0;
}
