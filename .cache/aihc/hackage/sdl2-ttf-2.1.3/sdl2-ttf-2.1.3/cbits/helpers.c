#include "SDL.h"
#include "SDL_ttf.h"

// Lots of SDL_ttf's render functions accept an SDL_Color directly. We send in
// a pointer instead, which we dereference here. Is there a way to avoid this?

// Note the "_p" added to the function names.

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Solid_p(
    TTF_Font *font,
    uint16_t *text,
    SDL_Color *fg) {

  return TTF_RenderUNICODE_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Shaded_p(
    TTF_Font *font,
    uint16_t *text,
    SDL_Color *fg,
    SDL_Color *bg) {

  return TTF_RenderUNICODE_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Blended_p(
    TTF_Font *font,
    uint16_t *text,
    SDL_Color *fg) {

  return TTF_RenderUNICODE_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUTF8_Solid_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg) {

  return TTF_RenderUTF8_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUTF8_Shaded_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg,
    SDL_Color *bg) {

  return TTF_RenderUTF8_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUTF8_Blended_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg) {

  return TTF_RenderUTF8_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderText_Solid_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg) {

  return TTF_RenderText_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderText_Shaded_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg,
    SDL_Color *bg) {

  return TTF_RenderText_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderText_Blended_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg) {

  return TTF_RenderText_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderGlyph_Solid_p(
    TTF_Font *font,
    uint16_t glyph,
    SDL_Color *fg) {

  return TTF_RenderGlyph_Solid(font, glyph, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderGlyph_Shaded_p(
    TTF_Font *font,
    uint16_t glyph,
    SDL_Color *fg,
    SDL_Color *bg) {

  return TTF_RenderGlyph_Shaded(font, glyph, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderGlyph_Blended_p(
    TTF_Font *font,
    uint16_t glyph,
    SDL_Color *fg) {

  return TTF_RenderGlyph_Blended(font, glyph, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUTF8_Blended_Wrapped_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg,
    uint32_t wrapLength) {

  return TTF_RenderUTF8_Blended_Wrapped(font, text, *fg, wrapLength);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Blended_Wrapped_p(
    TTF_Font *font,
    uint16_t *text,
    SDL_Color *fg,
    uint32_t wrapLength) {

  return TTF_RenderUNICODE_Blended_Wrapped(font, text, *fg, wrapLength);
}

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderText_Blended_Wrapped_p(
    TTF_Font *font,
    const char *text,
    SDL_Color *fg,
    uint32_t wrapLength) {

  return TTF_RenderText_Blended_Wrapped(font, text, *fg, wrapLength);
}
