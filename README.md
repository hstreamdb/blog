## How to add a new post

1. add the following to the beginning of you post (which must be a markdown file).
```markdown
---
title: XX XX XX XX XX
author: XX XX
---
```

2. add you markdown file under posts directory.

## How to render and view blog locally

```shell
$ cabal build
$ cabal exec site -- watch
```
