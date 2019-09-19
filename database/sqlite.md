# Database

An organized collection of data. The database management system (DBMS) is the software that interacts with end users, applications and the database itself to capture and analyze the data.

Formally, a "database" refers to a set of related data and the way it is organized. Access to this data is usually provided by a "database management system" (DBMS) consisting of an integrated set of computer software that allows users to interact with one or more databases and provides access to all of the data contained in the database (although restrictions may exist that limit access to particular data). The DBMS provides various functions that allow entry, storage and retrieval of large quantities of information and provides ways to manage how that information is organized.

Existing DBMSs provide various functions that allow management of a database and its data which can be classified into four main functional groups:

- _Data definition_: creation, modification and removal of _definitions that define the organization of the data_.

- _Update_: insertion, modification and deletion of the actual data.

- _Retrieval_: Providing information in a form directly usable or for further processing by other applications. 

- _Administration_: Registering and monitoring users, enforcing data security, monitoring performance, maintaining data integrity, dealing with concurrency control, and recovering information that has been corrupted by some event such as an unexpected system failure.

A linked-list system would be very inefficient when storing "sparse" databases where some of the data for any one record could be left empty. The relational model solved this by splitting the data into a series of normalized tables (or relations), with optional elements being moved out of the main table to where they would take up room only if needed. The relational part comes from entities referencing other entities in what is known as one-to-many relationship, like a traditional hierarchical model, and many-to-many relationship, like a navigational (network) model. Thus, a relational model can express both hierarchical and navigational models, as well as its native tabular model, allowing for pure or combined modeling in terms of these three models, as the application requires.  In the relational model, some bit of information was used as a "key", uniquely defining a particular record.

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

- `AND`, `OR`, `NOT`: `where` can be combined with these operators

```sql
SELECT column1, column2, ...
FROM table_name
WHERE condition1 AND condition2 AND condition3 ...;
```

```sql
SELECT * FROM Customers
WHERE Country='Germany' AND (City='Berlin' OR City='München');
```

- `ORDER BY`: sort the result set in ascending (by default) or descending order

```sql
SELECT column1, column2, ...
FROM table_name
ORDER BY column1, column2, ... ASC|DESC;
```

```sql
SELECT * FROM Customers
ORDER BY Country ASC, CustomerName DESC;
```

- `INSERT INTO`: insert new records in a table

```sql
INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);

INSERT INTO table_name
VALUES (value1, value2, value3, ...); 
```

```sql
INSERT INTO Customers (CustomerName, ContactName, Address, City, PostalCode, Country)
VALUES ('Cardinal', 'Tom B. Erichsen', 'Skagen 21', 'Stavanger', '4006', 'Norway');
```

- `NULL`: A field with a NULL value is a field with no value. It is not possible to test for NULL values with comparison operators, such as `=`, `<`, or `<>`. We use `IS NULL` or `IS NOT NULL`.

```sql
SELECT CustomerName, ContactName, Address
FROM Customers
WHERE Address IS NULL;
```

- `UPDATE`: modify the existing records in table

```sql
UPDATE table_name
SET column1 = value1, column2 = value2, ...
WHERE condition;
```

Without the `WHERE`, all records will be modified.

```sql
UPDATE Customers
SET ContactName = 'Alfred Schmidt', City= 'Frankfurt'
WHERE CustomerID = 1;

UPDATE Customers
SET ContactName='Juan'
WHERE Country='Mexico';
```

- `DELETE`: delete existing records in a table

```sql
DELETE FROM Customers WHERE CustomerName='Alfreds Futterkiste';

DELETE FROM table_name; # Delete all records in the table
```

- `SELECT TOP`: specify the number of records to return; in sqlite3, it's `select ... from ... (where ...) limit ...`;

```sql
SELECT column_name(s)
FROM table_name
WHERE condition
LIMIT number;
```

```sql
select * from employees where Country = 'Canada' limit 5
```

- `MIN()`, `MAX()`: the smallest/largest value of the selected column

```sql
select min(EmployeeId) from employees;
```

- `COUNT()`, `AVG()`, `SUM()`

```sql
select count(EmployeeId) from employees # 1
select sum(EmployeeId) from employees;  # 8
select avg(EmployeeId) from employees;  # 4.5
```

- `LIKE`: search for a specified pattern in a column; `%` represents zero, one or multiple characters, `_` represents a single character.

```sql
select * from employees where Country like 'C%';
```

- `IN`: specify multiple values in a `where` clause

```sql
select * from employees where EmployeeId in (1, 2);
select * from employees where EmployeeId not in (1, 2);
```

- `BETWEEN`: selects values within a given range

```sql
select * from employees where EmployeeId between 4 and 6;
select * from employees where EmployeeId not between 4 and 6;
```

- `AS`: give a table or a column in a table, a temporary name

```sql
select e.FirstName, e.LastName as Name, g.Name from employees as e, genres as g;
```


# SQLite Features

SQLite uses dynamic types for tables. You can store any value in any column regardless of the data type. SQLite allows a single database connection to access multiple database files simultaneously. SQLite is capable of creating in-memory databases which are very fast to work with.

SQLite stores the entire database (definitions, tables, indices, and the data itself) as a single cross-platform file on a host machine. It implements this simple design by locking the entire database file during writing. SQLite read operations can be multitasked, though writes can only be performed sequentially. SQLite is not the preferred choice for write-intensive deployments since it relies on file system locks. It has less knowledge of the other processes that are accessing the database at the same time.

## Serverless

In a client/server architecture, A separate server process is running. The applications that want to access the database server use TCP/IP protocol to send and receive requests.

SQLite does not require a server to run. SQLite database is integrated with the application that accesses the database. The applications interact with the SQLite database read and write directly from the database files stored on disk.

## Self-contained

SQLite quires minimal support from the OS or external library. An application that uses SQLite just is compiled with SQLite together. SQLite does not use any configuration files.

## [Transactional](https://en.wikipedia.org/wiki/Database_transaction)

All transactions in SQLite are fully ACID-compliant. It means all queries and changes are Atomic, Consistent, Isolated, and Durable.

## Zero-Configuration


No server process that needs to be configured, started and stopped. Does not use any configuration files.

# Usage

By default, an SQLite session uses the in-memory database. All changes will be gone when the session ends.
