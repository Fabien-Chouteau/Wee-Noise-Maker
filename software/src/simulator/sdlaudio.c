#include <SDL2/SDL.h>

extern void sdl_audio_callback(void *userdata, Uint8 *stream, int len);

int init_sdl_audio(int sample_rate, int sample_cnt)
{
  static SDL_AudioSpec want, have;

  if (SDL_Init(SDL_INIT_AUDIO) < 0) {
    fprintf(stderr, "Couldn't init SDL audio: %s\n", SDL_GetError());
    return 1;
  }

  SDL_zero(want);

  want.callback = sdl_audio_callback;
  want.userdata = NULL;
  want.format = AUDIO_S16;
  want.freq = sample_rate;
  want.channels = 2;
  want.samples = sample_cnt * want.channels;

  if (SDL_OpenAudio(&want, &have) < 0 ){
    fprintf(stderr, "Couldn't open audio: %s\n", SDL_GetError());
    return 1;
  }

  if (have.format != want.format) {
    fprintf(stderr, "Couldn't get requested audio format.");
    return 1;
  }

  if (have.freq != want.freq) {
    fprintf(stderr, "Couldn't get requested audio format.");
    return 1;
  }

  SDL_PauseAudio(0);

  return 0;
}
