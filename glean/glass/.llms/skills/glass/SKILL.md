---
description: >
  Load this skill when working with Glass — the code navigation service
  that powers CodeHub's go-to-definition, find-references, and symbol search.
  Glass is a Haskell service that queries Glean for code intelligence data.
oncalls:
  - code_indexing
apply_to_regex: 'glean/glass/'
apply_to_user_prompt: '(?i)(glass|glass.?server|glass.?cli|codehub.?glass|glean.?glass)'
---

# Glass

Glass is the code navigation backend for CodeHub. It serves symbol information, cross-references, and search results by querying Glean databases.

```
glass CLI  <-->  glass-server  <-->  glean
```

## Topic Routing

| Task | Reference |
|------|-----------|
| Testing Glass changes | [testing.md](testing.md) |
