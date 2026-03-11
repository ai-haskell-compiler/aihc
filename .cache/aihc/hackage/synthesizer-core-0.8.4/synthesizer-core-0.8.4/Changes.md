# Change log for the `synthesizer-core` package

## 8.1

* `Plain.Filter.Recursive.FirstOrder.highpassInit`,
  `Plain.Filter.Recursive.FirstOrder.highpassModifierInit`
  and derived functions change the meaning of the initial parameter.
  The previous meaning was pretty unclear and useless
  such that I consider it a bug.
  We do no longer negate the initial value.
  This is consistent with `lowpassInit`.
