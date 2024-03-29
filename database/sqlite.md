# Database

An organized collection of data. The database management system (DBMS) is the software that interacts with end users, applications and the database itself to capture and analyze the data.

Formally, a "database" refers to a set of related data and the way it is organized. Access to this data is usually provided by a "database management system" (DBMS) consisting of an integrated set of computer software that allows users to interact with one or more databases and provides access to all of the data contained in the database (although restrictions may exist that limit access to particular data). The DBMS provides various functions that allow entry, storage and retrieval of large quantities of information and provides ways to manage how that information is organized.

Existing DBMSs provide various functions that allow management of a database and its data which can be classified into four main functional groups:

- _Data definition_: creation, modification and removal of _definitions that define the organization of the data_.

- _Update_: insertion, modification and deletion of the actual data.

- _Retrieval_: Providing information in a form directly usable or for further processing by other applications. 

- _Administration_: Registering and monitoring users, enforcing data security, monitoring performance, maintaining data integrity, dealing with concurrency control, and recovering information that has been corrupted by some event such as an unexpected system failure.

A linked-list system would be very inefficient when storing "sparse" databases where some of the data for any one record could be left empty. The relational model solved this by splitting the data into a series of normalized tables (or relations), with optional elements being moved out of the main table to where they would take up room only if needed. The relational part comes from entities referencing other entities in what is known as one-to-many relationship, like a traditional hierarchical model, and many-to-many relationship, like a navigational (network) model. Thus, a relational model can express both hierarchical and navigational models, as well as its native tabular model, allowing for pure or combined modeling in terms of these three models, as the application requires.  In the relational model, some bit of information was used as a "key", uniquely defining a particular record.

## Basic Concepts

https://www.tutorialspoint.com/dbms/index.htm

- _Database_; _Data Definition Language_; _Data Modification Language_; _Database Management System_, which carries out _Data definition_, _Data Update_, _Data Retrieval_ and _User Administration_.

https://www.tutorialspoint.com/dbms/dbms_overview.htm

(https://www.geeksforgeeks.org/introduction-of-dbms-database-management-system-set-1/)[Intro to DBMS 1]

- why database: queries; data managed in objects and not files; controlling redundancy and inconsistency; efficient memory management and indexing to avoid file scans; concurrency control and transaction management; access control; ease in accessing data (through queries); integrity constraints; security;

https://www.geeksforgeeks.org/need-for-dbms/

https://www.geeksforgeeks.org/introduction-of-dbms-database-management-system-set-1/

https://www.geeksforgeeks.org/disadvantages-of-dbms/

- How DBMS is used; Interfaces

https://www.geeksforgeeks.org/use-of-dbms-in-system-software/

https://www.geeksforgeeks.org/interfaces-in-dbms/

- _ACID_: atomicity, durability, isolation, consistency. To ensure efficient transaction without data corruption.

- Architecture: 3-Tier (Presentation - Application - Database); 2-tier (basically a client-server model);

https://www.tutorialspoint.com/dbms/dbms_architecture.htm

https://www.geeksforgeeks.org/dbms-architecture-2-level-3-level/

- Level: physical; conceptual; external

https://www.geeksforgeeks.org/introduction-of-3-tier-architecture-in-dbms-set-2/

## Data Models

How the logical structure of a database is modeled

https://www.tutorialspoint.com/dbms/dbms_data_models.htm

- _Entity-Relationship Model_: based on the notion of real-world entities and relationships among them

- _Relational Model_: based on first-order predicate logic and defines a table as an n-ary relation

## Database Schema

the skeleton structure that represents the logical view of the entire database

- database schema

- database instance

## Database Independence

https://www.tutorialspoint.com/dbms/dbms_data_independence.htm

- Logical Data Independence

- Physcial Data Independence

## Database Objects

https://www.geeksforgeeks.org/database-objects-in-dbms/

Anything made from _create command_, used to hold and manipulate the data.

- *Table*: basic unit of storage; composed of rows and columns

- *View*: logiclaly represents subsets of data from one or more tables

- *Sequence*: generates primary key values

- *Index*: improves the performance of some queries

- *Synonym*: alternative name for an object

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

- A `JOIN` clause is used to combine rows from two or more tables, based on related column between them. There are inner, left (outer), right (outer) and full (outer) joins.

The `INNER JOIN` selects records that have matching values in both tables.

```sql
select column_names
from table1
inner join table2
on table1.column_name = table2.column_name
```

```sql
select distinct artists.ArtistId, artists.Name
from artists
inner JOIN albums on artists.Artistid = albums.ArtistId;
```

The `LEFT JOIN` keyword returns all records from the left table (table1), and the matched records from the right table (table2). The result is `NULL` from the right side, if there is no match. The same for `RIGHT JOIN`.

The `FULL OUTER JOIN` keyword return all records when there is a match in left (table1) or right (table2) table records.

```sql
SELECT A.CustomerName AS CustomerName1, B.CustomerName AS CustomerName2, A.City
FROM Customers A, Customers B
WHERE A.CustomerID <> B.CustomerID
AND A.City = B.City 
ORDER BY A.City;


select a.Name as Name1, b.Name as Name2, b.GenreId from tracks a, tracks b
where a.Name <> b.Name and a.GenreId = b.GenreId;
```

- `UNION`: combine the result-set of two or more `SELECT` statements; Both the result-sets must have the same number of columns and also similar data types for each column.

```sql
SELECT City FROM Customers
UNION
SELECT City FROM Suppliers
ORDER BY City;

SELECT Address, City FROM Customers
UNION
SELECT Address, City FROM Suppliers
ORDER BY City;
```

- `GROUP BY`: The `GROUP BY` statement groups rows that have the same values into summary rows

```sql
SELECT COUNT(CustomerID), Country
FROM Customers
GROUP BY Country
ORDER BY COUNT(CustomerID) DESC;

SELECT Shippers.ShipperName,COUNT(Orders.OrderID) AS NumberOfOrders FROM Orders
JOIN Shippers ON Orders.ShipperID = Shippers.ShipperID
GROUP BY ShipperName;
```

- `HAVING`: `WHERE` could not be used with aggregate functions

```sql
SELECT COUNT(CustomerID), Country
FROM Customers
GROUP BY Country
HAVING COUNT(CustomerID) > 5;

SELECT Employees.LastName, COUNT(Orders.OrderID) AS NumberOfOrders
FROM (Orders
INNER JOIN Employees ON Orders.EmployeeID = Employees.EmployeeID)
GROUP BY LastName
HAVING COUNT(Orders.OrderID) > 10;
```

- `EXISTS`: used to test for the existence of any record in a subquery

```sql
SELECT SupplierName

FROM Suppliers
WHERE EXISTS (SELECT ProductName FROM Products WHERE Products.SupplierID = Suppliers.supplierID AND Price < 20);
```

- `ANY`, `ALL`: used with a `WHERE` or `HAVING` clause.

```sql
SELECT ProductName 
FROM Products
WHERE ProductID = ANY (SELECT ProductID FROM OrderDetails WHERE Quantity = 10);
```

`SELECT INTO`: copies data from one table into a new table.

- `INSERT INTO SELECT`: copies data from one table and inserts it into another

```sql
INSERT INTO Customers (CustomerName, City, Country)
SELECT SupplierName, City, Country FROM Suppliers
WHERE Country='Germany';
```

- `CASE`: goes through conditions and returns a value when the first condition is met

```sql
SELECT OrderID, Quantity,
CASE
    WHEN Quantity > 30 THEN "The quantity is greater than 30"
    WHEN Quantity = 30 THEN "The quantity is 30"
    ELSE "The quantity is under 30"
END AS QuantityText
FROM OrderDetails;

SELECT CustomerName, City, Country
FROM Customers
ORDER BY
(CASE
    WHEN City IS NULL THEN Country
    ELSE City
END);
```

- `IFNULL`: return an alternative if an expression is `NULL`; `COALESCE`: returns a copy of its first non-NULL argument or NULL if all arguments are `NULL`.

```sql
SELECT ProductName, UnitPrice * (UnitsInStock + IFNULL(UnitsOnOrder, 0))
FROM Products;

SELECT ProductName, UnitPrice * (UnitsInStock + COALESCE(UnitsOnOrder, 0))
FROM Products;
```

- A stored procedure is a prepared SQL code that you can save, so the code can be reused over and over again.

```sql
CREATE PROCEDURE SelectAllCustomers
AS
SELECT * FROM Customers
GO;

EXEC SelectAllCustomers;

CREATE PROCEDURE SelectAllCustomers @City nvarchar(30)
AS
SELECT * FROM Customers WHERE City = @City
GO;

EXEC SelectAllCustomers @City = "London";
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
