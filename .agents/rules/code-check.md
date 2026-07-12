---
trigger: always_on
glob:
description: Run all source code validation checks with `just validate`
---

After editing any source code, you must run `just validate`, which will output diagnostics collected from running all source code validation checks. You must iteratively address these diagnostics and re-run `just validate` until all diagnostics have been addressed.

