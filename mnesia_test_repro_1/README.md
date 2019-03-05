# Description

This reproduces a deadlock due to an atomicity violation between `merge_schema` TXs when starting mnesia, and an `add_table_copy` TX.

For example, suppose there are two nodes A and B:

Starting mnesia on B will produce two `merge_schema` TXs (1 and 2) before it’s fully functional,
but mnesia on B will start handling other TXs after TX 1.

If we call `add_table_copy(Tab, B, disc_copies)` on A in between TX 1 and TX 2, it will produce a new TX 3 such that:

1. TX 3 grabs the schema lock, and waits for mnesia on B to be fully functional before it can commit.
2. TX 2 waits for TX 3 on the schema lock

Thus neither mnesia on B will become ready or the lock will be released, which is a deadlock.
