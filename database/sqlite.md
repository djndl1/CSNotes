# SQL (Structured Query Language)

SQL accesses and manipulates databases. It executes queries against a database; retrieve data from a database; insert records in a database; update records in a database; delete records in a database; create new database; create new tables in a database; create views in a database; sets permissions on tables, procedures and views.

RDMBS (relational Database Management System) is the basis for SQL and all modern database systems. The data in RDBMS is stored in database objects called _tables_, a collection of related data entries and it  consists of columns and rows. Each table is broken up into smaller entities called _fields_, a column in a table that is designed to maintain specific information about every record (a row, each individual entry that exists in a table) in the table.

A database contains one or more tables. Each table is identified with a name. Each table contain records with data.

## Syntax

Most of the actions you need to perform on a database are done with SQL statements. SQL keywords are case-insensitive. Semicolon is the standard way to separate each SQL statement in database systems that allow more than one SQL statement to be executed in the same call to the server.

- `SELECT`: select data from a database. The data returned is stored in a result table called the _result set_.

```sql
SELECT column1 column2, ... FROM table_name;
```

- `SELECT DISTINCT`: return only distinct values

```sql
SELECT DISTINCT column1, column2, ...
FROM table_name;
```

```sql
SELECT Count(*) AS DistinctCountries
FROM (SELECT DISTINCT Country FROM Customers);
```

- `WHERE`: filter records

```sql
SELECT column1, column2, ...
FROM table_name
WHERE condition;
```

```sql
SELECT * FROM Customers
WHERE Country='Mexico';
```

`=` (equal), `>`, `<`, `>=`, `<=`, `<>` (not equal), `BETWEEN`, `LIKE` (a pattern), `IN` (multiple possible values for a field)

# SQLite Features

SQLite uses dynamic types for tables. You can store any value in any column regardless of the data type. SQLite allows a single database connection to access multiple database files simultaneously. SQLite is capable of creating in-memory databases which are very fast to work with.

## Serverless

In a client/server architecture, A separate server process is running. The applications that want to access the database server use TCP/IP protocol to send and receive requests.

SQLite does not require a server to run. SQLite database is integrated with the application that accesses the database. The applications interact with the SQLite database read and write directly from the database files stored on disk.

## Self-contained

SQLite quires minimal support from the OS or external library. An application that uses SQLite just is compiled with SQLite together. SQLite does not use any configuration files.

## Transactional

All transactions in SQLite are fully ACID-compliant. It means all queries and changes are Atomic, Consistent, Isolated, and Durable.

## Zero-Configuration

No server process that needs to be configured, started and stopped. Does not use any configuration files.

# Usage

By default, an SQLite session uses the in-memory database. All changes will be gone when the session ends.
