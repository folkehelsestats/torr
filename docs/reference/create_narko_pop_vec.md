# Apply `create_narko_pop()` Over Multiple Specifications

A convenience wrapper that applies
[`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md)
repeatedly to the same dataset for a set of `(vars, val)`
specifications. This helps avoid writing multiple sequential calls and
keeps analysis code concise and consistent.

## Usage

``` r
create_narko_pop_vec(D, vars_list, vals, ...)
```

## Arguments

- D:

  A data.frame or data.table to be augmented. The object is passed to
  and returned from each call to
  [`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md).

- vars_list:

  A list where each element is a character vector of variable names to
  pass as `vars` to
  [`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md).
  For example: `list(c("ans2_a","ans3_1"), c("ans2_b","ans3_2"))`.

- vals:

  A character vector of the same length as `vars_list`. Each element is
  passed as `val` to
  [`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md)
  for the corresponding `vars_list` entry.

- ...:

  Additional arguments passed through to
  [`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md),
  if supported by that function (e.g., `types`, `ans`, etc.).

## Value

Returns the updated object `D` after all calls to
[`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md)
have been applied.

## Details

Internally, the function loops once over `seq_along(vals)`, updating `D`
in-place for each `(vars, val)` pair by calling
[`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md).
The order of `vars_list` / `vals` determines the order of application.

Basic validation is performed to ensure:

- `vars_list` is a list.

- `vals` is a character vector with the same length as `vars_list`.

- Each element of `vars_list` is a non-empty character vector.

## See also

[`create_narko_pop()`](https://github.com/folkehelsestats/torr/reference/create_narko_pop.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example specification (mirrors repeated calls):
vars_list <- list(
  c("ans2_a", "ans3_1"),
  c("ans2_b", "ans3_2"),
  c("ans2_c", "ans3_3"),
  c("ans2_e", "ans3_5"),
  c("ans2_f", "ans3_6"),
  c("ans2_g", "ans3_7"),
  c("ans2_h", "ans3_8")
)
vals <- c("kokain", "mdma", "amfetaminer", "heroin", "ghb", "lsd", "annet")

DD2 <- torr::create_narko_pop_vec(DD, vars_list, vals)
} # }
```
