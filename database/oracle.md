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