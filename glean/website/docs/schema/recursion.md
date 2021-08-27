---
id: recursion
title: Recursion
sidebar_label: Recursion
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

Predicates can be (mutually) recursive. In other words, **we can use
predicates to define recursive types**. You can define linked lists,
trees and DAGs using predicates. However, the **facts cannot form a
cycle**: we cannot have a circular list, or a cyclic graph in the
data.


The restriction on cyclic data is enforced when facts are added to a
database: each new fact added to the database can only refer to
earlier facts via its key. This is because facts are added to the
database in batches, removing duplicates and substituting references
to duplicated facts at the same time. Allowing recursion between the
keys would make this process significantly harder.


Facts can be recursive in their *values*, but not their *keys*. A
mutually recursive set of facts must be added to the database in a
single batch, however.

To summarise, recursion is

* allowed between *predicates*
* not allowed between *keys*
* allowed between *values*
