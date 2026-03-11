# Changelog for pcre-light

## 0.4.1.3
- Remove old-base flag.
- Fix [#18](https://codeberg.org/daniel-casanueva/pcre-light/issues/18).
- Metadata update.

## 0.4.1.2
- Replace finalizerFree with c_pcre_free. ([PR 17](https://gitlab.com/daniel-casanueva/haskell/pcre-light/-/merge_requests/17))

## 0.4.1.1
- Bugfix where ByteString.empty was treated differently than the empty ByteString `""`

## 0.4.1.0
- Add `captureCount` and `captureNames` for working with named captures/groups
