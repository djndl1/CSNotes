# Relational Database

Typically  A DBMS has kernel code (manages memory and storage for the DBMS), repository of metadata (_data dictionary_) and query language. A relational database has structures, operaitons and integrity rules.


[_relation_](https://en.wikipedia.org/wiki/Relation_(database)): a set of tuples.

An RDBMS distinguishes between logical operations and physical operations (how things should be done by the RDBMS).

_Schema_: a collection of logical data structures (schema objects). A database user owns a database schema: _tables_, _indexes_.

Oracle database can store program units written in Java in addition to PL/SQL.

_transaction_: logical atomic unit of work that contains one or more SQL statements. It eithre all succeeds or fails as a whole.
Concurrent access is done by using locks. Oracle enforces statement-level read consistency.

# Basics

- `DUAL` table: used for queries that do not require any table, probably not even accessed.

- `ORDER BY`: can be used with column position or even the result of a function or expression.

```sql
SELECT
    name,
    credit_limit
FROM
    customers
ORDER BY
    2 DESC,
    1;
```

- By default, Oracle capitalizes the column heading in the query result. The alias can be quoted to avoid this. Also, when special characters are present, quotes are also needed.

- oclumn alias can also be used for an expression.

```sql
SELECT
  first_name  || ' '  || last_name
FROM
  employees;
  
SELECT
  product_name,
  list_price - standard_cost AS gross_profit
FROM
  products;
```

An alias name must be used after the table is assigned the alias.

- (12c) `FETCH`: similar to `LIMIT` in MySQL and PostgreSQL.

```sql
[ OFFSET offset ROWS] --- the number of rows to skip, negative values are treated as 0
 FETCH  NEXT [  row_count | percent PERCENT  ] ROWS  [ ONLY | WITH TIES ] --- only: exact N rows; with ties: additional rows with the same sort key as the last row fetched (the columns of the first N rankings)
```

- `BETWEEN` can also used with date.

- `ESCAPE` can be used if needed with pattern.

- join clause: `ON` a predicate, or `USING` a column for equality test

- cross join makes the Cartesion product of two sets, useful when generating data for testing.

- self join can be used to compare rows within the same table.

```sql
--- all employees who hve the same hire dates
SELECT
   e1.hire_date,
  (e1.first_name || ' ' || e1.last_name) employee1,
  (e2.first_name || ' ' || e2.last_name) employee2  
FROM
    employees e1
INNER JOIN employees e2 ON
    e1.employee_id > e2.employee_id
    AND e1.hire_date = e2.hire_date
ORDER BY  
   e1.hire_date DESC,
   employee1, 
   employee2;
```

- `GROUP BY` can be used with an expression. `HAVING` filters groups of rows returned by `GROUP BY`, otherwise it is just a `WHERE`.

```sql
SELECT
    EXTRACT(YEAR FROM order_date) YEAR,
    COUNT( order_id )
FROM
    orders
GROUP BY
    EXTRACT(YEAR FROM order_date)
ORDER BY
    YEAR;
```

- Multilevel grouping is done by using `ROLLUP` following `GROUP BY`. `ROLLUP` is a better and faster way to calculate the grand total. It calculates multiple levels of subtotals across a group of columns or dimenson along with the grand total.

```sql
SELECT
   salesman_id,
   customer_id,
   SUM(quantity * unit_price) amount
FROM
   orders
INNER JOIN order_items USING (order_id)
WHERE
   status      = 'Shipped' AND 
   salesman_id IS NOT NULL AND 
   EXTRACT(YEAR FROM order_date) = 2017
GROUP BY
   ROLLUP(salesman_id, customer_id);
```

- `UNION`/`UNION ALL`: the latter retains duplicates.

- `ANY (subquery)`/`SOME`: evalutes to false if the subquery returns no rows

- cascade delete is done when the foreign key of the table is constrained by delete cascade.

- `TRUNCATE TABLE` is used to delete all rows in a large table.

- To check whether a column exists in a table, query teh data from `user_tab_cols` view.

```sql
SELECT
    COUNT(*)
FROM
    user_tab_cols
WHERE
    column_name = 'FIRST_NAME'
    AND table_name = 'MEMBERS';
```

- For virtual columns, only metadata is stored. The values are always in sync with the source columns. However, their values are calculated t runtime. Virtual columns are supported only inlational heap tables. To query the names of virtual columns in a table.

```sql
SELECT 
    column_name, 
    virtual_column,
    data_default
FROM 
    all_tab_cols
WHERE owner = 'OT' 
AND table_name = 'PARTS';
```

- Query unused columns from `DBA_UNUSED_COL_TABS`.

- check non null by querying `user_constraints`. Before adding `Not Null` constraint, make sure that the existing data violates the constraint.

```sql
UPDATE
    surcharges
SET
    amount = 0
WHERE
    amount IS NULL;
```

- Pay attention to date format `nls_date_format`, `nls_date_language`,

# OracleDB administration

## Architecture

_database instance_: the combination of memory and processes that are a part of running installation.

_database_: a set of files that store data. An instance can mount only one database at a single point time. Multiple can access the same database (like in a clustering environment)

Multitenant architecture; CDB; PDB; The application model might include definitions of tables, views, user accounts, and PL/SQL packages that are common to a set of PDBs. An _application container_ functions as an application-specific CDB within a CDB. 

Sharding Architecture; horizontal partitioning of data across multiple Oracle databases. In a sharding architecture, each database is hosted on a dedicated server with its own local resources - CPU, memory, flash, or disk. Each database in such configuration is called a shard. All of the shards together make up a single logical database, which is referred to as a sharded database.
 
## Application Architecture

- Client-Server

- Multitier archtecture: the application server can serve as an interface between clients and multiple databases and provides an additional level of security.

- Simple Oracle Document Access

## Oracle Net Services Architecture

_Oracle Net Listener_: a process that runs on the database or elsewhere in the network. Sharding Architecture; horizontal partitioning of data across multiple Oracle databases. In a sharding architecture, each database is hosted on a dedicated server with its own local resources - CPU, memory, flash, or disk. Each database in such configuration is called a shard. All of the shards together make up a single logical database, which is referred to as a sharded database.

- dedicated server architecture

- shared server architecture: a pool of shared server processes for multiple sessions. A client process communicates with a dispatcher.

### Physical storage structures

1. data files

2. control files: metadata that describes the physical structure of the database including the database name and the locations.

3. online redo log files: two or more. Made up of redo entries that record all changes made to the data.

### Logical Storage Structures

1. data/logical blocks: a number of bytes on the disk.

2. extents: a specific number of logically contiguous data blocks.

3. segments: a set of extents allocated for storing database objects.

4. tablespaces: logical container for a segment. Each tablespace consists of one or more datafiles.

### Database Instance

1. Systme Global Area: a shared memory structure allocated when running, containing data and control information, shared by the multiple independent processes such pmon, smon, mmon, lgwr etc. 

2. Program Global Area: private memory area for each session.

### Background processes

PMON, SMON, BSWn, CKPT, LGWR, ARCn, MMON, MMAN, LREG.

# Data Pump

`expdp`, `impdp`,  the Data Pump API and the Metadata API. All related operations are done on the server side.

Create a directory and grant read write permissions on it to the user before `expdp`.

```sql

CREATE OR REPLACE DIRECTORY test_dir AS '/u01/app/oracle/oradata/';
GRANT READ, WRITE ON DIRECTORY test_dir TO scott;
```

```shell
expdp scott/tiger@db10g schemas=SCOTT directory=TEST_DIR dumpfile=SCOTT.dmp logfile=expdpSCOTT.log

impdp scott/tiger@db10g schemas=SCOTT directory=TEST_DIR dumpfile=SCOTT.dmp logfile=impdpSCOTT.log
```
