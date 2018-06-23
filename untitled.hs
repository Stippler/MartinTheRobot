-- DOCUMENTATION --

SDL_LoadWAV: 
This function, if successfully called, returns a pointer to an SDL_AudioSpec structure filled with the audio data format of the wave source data. audio_buf is filled with a pointer to an allocated buffer containing the audio data, and audio_len is filled with the length of that audio buffer in bytes.

This function returns NULL if the wave file cannot be opened, uses an unknown data format, or is corrupt; call SDL_GetError() for more information.

When the application is done with the data returned in audio_buf, it should call SDL_FreeWAV() to dispose of it. 
