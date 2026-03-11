## [1.0.0.1] - April 10, 2025

- Support base-4.21

## [1.0.0] - November 14, 2023

- Rework `Tb_attr` and `Tb_color` into `Tb_attrs`
- Rework `Tb_select_input_mode` and `Tb_input_mode`
- Rework `Tb_select_output_mode` and `Tb_output_mode`
- Make `mod` field of `Tb_event` a `Maybe Tb_event_mod` rather than a `Tb_event_mod`
- Make `Show` instance of `Tb_key` not call `error` on unknown keys
- Improve `Show` instance of `Tb_event_mod`, `Tb_input_mode`
- Hide representation of `Tb_event_mod`
- Add `Exception` instance for `Tb_init_error`
- Remove `tb_attr`
- Remove `Ord` instances of `Tb_cell`, `Tb_event`, `Tb_event_mod`, `Tb_event_type`, `Tb_init_error`, `Tb_input_mode`, `Tb_output_mode`

## [0.1.1] - November 5, 2023

- Add `Monoid` instance for `Tb_attr`

## [0.1.0] - October 25, 2022

- Initial release
