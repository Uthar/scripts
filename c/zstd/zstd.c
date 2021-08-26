#include <stdint.h>
#include <unistd.h>
#include <stdio.h>

#include <zstd.h>

int main(int argc, char **argv) {
  int bufsz = 32 * 4096;
  uint8_t inbuf[bufsz];
  uint8_t outbuf[bufsz];

  ZSTD_inBuffer in;
  in.src = inbuf;
  in.size = bufsz;
  in.pos = 0;

  ZSTD_outBuffer out;
  out.dst = outbuf;
  out.size = bufsz;
  out.pos = 0;

  int nin = 0;
  int done = 0;
  ZSTD_CCtx * ctx = ZSTD_createCCtx();

  while (!done) {
      nin = read(0, inbuf, bufsz);
      ZSTD_EndDirective flag = ZSTD_e_continue;
      if (in.pos==in.size){
          flag = ZSTD_e_flush;
          in.pos = 0;
      }
      if (!nin) {
          flag = ZSTD_e_end;
      }
      in.size = nin;
      int ret = ZSTD_compressStream2(ctx, &out, &in, flag);
      if (ZSTD_isError(ret)){
          printf("ZSTD error: %d",ret);
          _exit(1);
      }
      done = !nin && ret == 0;
      if (out.pos == out.size || done){
          int written = 0;
          write(1,outbuf,out.pos);
          out.pos = 0;
      }

  }
}
