
-- 1. Selectează numele și adresa clienților împreună cu totalul comenzilor lor, dar doar pentru clienții din segmentul "AUTOMOBILE" care au comenzi peste 1000.
-- 1. Select complex fara grupare

SELECT c.c_name, c.c_address, o.o_totalprice FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
WHERE o.o_totalprice > 1000 AND c.c_mktsegment = 'AUTOMOBILE';
-- 2. Selectează numele și adresa furnizorilor care au un sold mai mare decât media tuturor furnizorilor și care aparțin unei națiuni al cărei nume începe cu "A".
-- 2. Select cu predicat dublu AND si subquery

SELECT s.s_name, s.s_address FROM supplier s 
WHERE s.s_acctbal > (SELECT AVG(s_acctbal) FROM supplier) AND s_nationkey IN (SELECT n_nationkey FROM nation WHERE n_name LIKE 'A%');

-- 3. Selectează comenzile și numele clienților pentru comenzile cu un total peste 50.000 sau pentru clienții din segmentul "BUILDING".
-- 3. Select cu predicat dublu OR si JOIN

SELECT o.o_orderkey, o.o_totalprice, c.c_name FROM orders o 
JOIN customer c ON o.o_custkey = c.c_custkey 
WHERE o.o_totalprice > 50000 OR c.c_mktsegment = 'BUILDING';

-- 4. Numără câte națiuni sunt per regiune și calculează media valorilor n_regionkey.
-- 4. Grupare dupa un camp cu COUNT si AVG

SELECT n.n_regionkey, r.r_name, COUNT(*) AS total_nations, AVG(n.n_regionkey) AS avg_region 
FROM nation n 
JOIN region r ON n.n_regionkey = r.r_regionkey 
GROUP BY n.n_regionkey, r.r_name;

-- 5. Selectează numele clienților, totalul comenzilor și națiunea lor pentru comenzile care sunt deschise (o_orderstatus = 'O').
-- 5. Inner Join intre mai multe tabele
SELECT c.c_name, o.o_totalprice, n.n_name FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey xxx
JOIN nation n ON c.c_nationkey = n.n_nationkey 
WHERE o.o_orderstatus = 'O';

-- 7. Selectează numele furnizorului, națiunea și costul aprovizionării, dar include și acei furnizori care nu au legătură directă cu aprovizionarea (partsupp).
-- 7. Left Join complex
SELECT s.s_name, n.n_name, ps.ps_supplycost FROM supplier s 
LEFT JOIN nation n ON s.s_nationkey = n.n_nationkey 
LEFT JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey 
WHERE ps.ps_supplycost > 500;

-- 8. Combină părțile și furnizorii care le aprovizionează, dar păstrează doar acele piese care au un preț mai mare decât media tuturor pieselor.
-- 8. Full Join cu conditie complexa
SELECT ps.ps_partkey, p.p_name, s.s_name FROM partsupp ps 
FULL JOIN part p ON ps.ps_partkey = p.p_partkey 
FULL JOIN supplier s ON ps.ps_suppkey = s.s_suppkey 
WHERE p.p_retailprice > (SELECT AVG(p_retailprice) FROM part);

-- 9. Select cu trei JOIN-uri și filtrare avansată
-- 9. Afișează clienții, comenzile lor, națiunea și regiunea, dar doar pentru comenzile mai mari de 10.000 și din regiunile 'AMERICA' sau 'EUROPE'.
SELECT c.c_name, o.o_totalprice, n.n_name, r.r_name FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
JOIN nation n ON c.c_nationkey = n.n_nationkey 
JOIN region r ON n.n_regionkey = r.r_regionkey 
WHERE o.o_totalprice > 10000 AND r.r_name IN ('AMERICA', 'EUROPE');

-- 10. Adaugă și lineitem, astfel încât să includă și prețul extins (l_extendedprice) al fiecărei linii de comandă, dar doar pentru comenzile cu discount mai mare de 5%.
-- 10. Select cu patru JOIN-uri și condiții
SELECT c.c_name, o.o_totalprice, n.n_name, r.r_name, l.l_extendedprice FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
JOIN nation n ON c.c_nationkey = n.n_nationkey 
JOIN region r ON n.n_regionkey = r.r_regionkey 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
WHERE l.l_discount > 0.05;

-- 11. Afișează totalul vânzărilor pentru fiecare status de comandă, dar doar pentru statusurile unde totalul vânzărilor depășește 1.000.000.
-- 11. Grupare cu SUM și filtrare
SELECT o_orderstatus, SUM(o_totalprice) AS total_sales FROM orders 
GROUP BY o_orderstatus HAVING SUM(o_totalprice) > 1000000;

-- 12. Numără furnizorii pe națiuni, dar păstrează doar acele națiuni care au mai mulți furnizori decât minimul din toate națiunile.
-- 12. Grupare cu COUNT si filtrare nested
SELECT s.s_nationkey, n.n_name, COUNT(*) 
FROM supplier s 
JOIN nation n ON s.s_nationkey = n.n_nationkey 
GROUP BY s.s_nationkey, n.n_name 
HAVING COUNT(*) > (SELECT MIN(count) FROM (SELECT COUNT(*) AS count FROM supplier GROUP BY s_nationkey) sub);

-- 13. Afișează prețul mediu extins pentru fiecare l_returnflag, dar doar dacă depășește media tuturor prețurilor.
-- 13. Grupare cu AVG și filtru pe medie
SELECT l_returnflag, AVG(l_extendedprice) AS avg_price FROM lineitem 
GROUP BY l_returnflag HAVING AVG(l_extendedprice) > (SELECT AVG(l_extendedprice) FROM lineitem);

-- 14. Calculează prețul final ((l_extendedprice * (1 - l_discount) * (1 + l_tax))) pentru liniile de comandă între 1994 și 1995.
-- 14. Select cu calcul, alias și filtrare avansată
SELECT l_orderkey, (l_extendedprice * (1 - l_discount) * (1 + l_tax)) AS final_price FROM lineitem 
WHERE l_shipdate BETWEEN '1994-01-01' AND '1995-12-31';

-- 15. Selectează toate statusurile distincte de comenzi, dar doar pentru comenzile cu total peste 20.000.
-- 15. Select cu DISTINCT și filtrare
SELECT DISTINCT o_orderkey, o_orderstatus, o_totalprice FROM orders WHERE o_totalprice > 20000;

-- Returnează primii 20 de clienți ordonați descrescător după sold, sărind primii 10.
-- 16. Select cu LIMIT, OFFSET si ORDER BY
SELECT * FROM customer ORDER BY c_acctbal DESC LIMIT 20 OFFSET 10;

-- 17. Returnează numele clienților care au făcut comenzi cu un total mai mare decât media tuturor comenzilor.
-- 17. Select cu subquery si agregare
SELECT c_name FROM customer WHERE c_custkey IN (SELECT o_custkey FROM orders WHERE o_totalprice > (SELECT AVG(o_totalprice) FROM orders));

-- 18. Creează o coloană nouă care etichetează clienții ca 'VIP' dacă au soldul mai mare decât media tuturor clienților, altfel 'Regular'.
-- 18. Select cu CASE si grupare
SELECT c_name, CASE WHEN c_acctbal > (SELECT AVG(c_acctbal) FROM customer) THEN 'VIP' ELSE 'Regular' END AS customer_status FROM customer;

-- 19. Select cu COUNT si DISTINCT
-- 19.  Numărul total de clienți.
SELECT COUNT(DISTINCT c_custkey) AS total_customers FROM customer;

-- 20. Select cu MIN si MAX
-- 20. Valoarea minimă și maximă a `c_acctbal`.
SELECT MIN(c_acctbal) AS min_balance, MAX(c_acctbal) AS max_balance FROM customer;

-- 21. Select cu SUM si GROUP BY
-- 21. Suma comenzilor pentru fiecare segment de piață.
SELECT c_mktsegment, SUM(o_totalprice) AS total_revenue FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
GROUP BY c_mktsegment;

-- 22. Select cu HAVING si COUNT
-- 22. Lista segmentelor de piață care au mai mult de 10 clienți.
SELECT c_mktsegment, COUNT(*) AS num_customers FROM customer 
GROUP BY c_mktsegment HAVING COUNT(*) > 10;

-- 23. Subinterogare corelată
-- 23. Lista clienților care au făcut comenzi mai mari decât media comenzilor.
SELECT c_name FROM customer WHERE c_custkey IN 
(SELECT o_custkey FROM orders WHERE o_totalprice > (SELECT AVG(o_totalprice) FROM orders));

-- 24. JOIN între patru tabele cu filtrare avansată
-- 24. Lista comenzilor și clienților din America cu valoare mare.
SELECT c.c_name, o.o_totalprice, n.n_name, r.r_name FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
JOIN nation n ON c.c_nationkey = n.n_nationkey 
JOIN region r ON n.n_regionkey = r.r_regionkey 
WHERE o.o_totalprice > 20000 AND r.r_name = 'AMERICA';

-- 25. Subinterogare cu EXISTS
-- 25. Lista furnizorilor eligibili.
SELECT s_name FROM supplier s WHERE EXISTS 
(SELECT 1 FROM partsupp ps WHERE ps.ps_suppkey = s.s_suppkey AND ps.ps_supplycost > (SELECT AVG(ps_supplycost) FROM partsupp));

-- 26. Select cu WINDOW FUNCTION
-- 26. O listă de comenzi cu total și medie mobilă.
SELECT c.c_name, o.o_totalprice, AVG(o.o_totalprice) OVER (PARTITION BY c.c_custkey) AS moving_avg 
FROM customer c JOIN orders o ON c.c_custkey = o.o_custkey;

-- 27. Select cu CTE (Common Table Expression)
-- 27. Lista clienților care au plasat comenzi semnificative.
WITH HighValueOrders AS (
    SELECT o_custkey FROM orders WHERE o_totalprice > 10000
)
SELECT c.c_name, c.c_address FROM customer c 
WHERE c.c_custkey IN (SELECT o_custkey FROM HighValueOrders);

-- 28. Select cu UNION
-- 28. Lista unificată de clienți din aceste două regiuni.
SELECT c_name FROM customer c JOIN nation n ON c.c_nationkey = n.n_nationkey 
JOIN region r ON n.n_regionkey = r.r_regionkey WHERE r.r_name = 'AMERICA'
UNION
SELECT c_name FROM customer c JOIN nation n ON c.c_nationkey = n.n_nationkey 
JOIN region r ON n.n_regionkey = r.r_regionkey WHERE r.r_name = 'EUROPE';

-- 29. SELF JOIN pentru identificarea clienților care au aceeași națiune
-- 29. Listează numele perechilor de clienți care aparțin aceleiași națiuni.
SELECT a.c_name AS client1, b.c_name AS client2, a.c_nationkey 
FROM customer a 
JOIN customer b ON a.c_nationkey = b.c_nationkey AND a.c_custkey < b.c_custkey;

-- 30. FULL OUTER JOIN între comenzi și linii de comandă
-- 30. O listă de comenzi și linii de comandă, inclusiv cele fără potrivire.
SELECT o.o_orderkey, o.o_totalprice, l.l_partkey, l.l_extendedprice 
FROM orders o 
FULL OUTER JOIN lineitem l ON o.o_orderkey = l.l_orderkey;

-- 31. Utilizarea PARTITION BY și RANK pentru a găsi cei mai buni clienți per regiune
-- 31. Lista clienților și clasamentul lor per regiune.
SELECT c.c_name, r.r_name, SUM(o.o_totalprice) AS total_spent,
RANK() OVER (PARTITION BY r.r_name ORDER BY SUM(o.o_totalprice) DESC) AS ranking
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN nation n ON c.c_nationkey = n.n_nationkey
JOIN region r ON n.n_regionkey = r.r_regionkey
GROUP BY c.c_name, r.r_name;

-- 32. EXCEPT pentru identificarea clienților care nu au făcut comenzi
-- 32. Lista clienților fără comenzi.
SELECT c_name FROM customer
EXCEPT
SELECT c_name FROM customer JOIN orders ON customer.c_custkey = orders.o_custkey;

-- 33. INTERSECT pentru identificarea clienților care au plasat și comenzi mari și comenzi mici
-- 33. Lista clienților care au comandat în ambele categorii.
SELECT c_name FROM customer JOIN orders ON customer.c_custkey = orders.o_custkey WHERE o_totalprice > 50000
INTERSECT
SELECT c_name FROM customer JOIN orders ON customer.c_custkey = orders.o_custkey WHERE o_totalprice < 5000;

-- 34. Select cu mai multe JOIN-uri și filtrare avansată
-- 34. Lista comenzilor mari și detalii despre clienții lor.
SELECT c.c_name, o.o_orderkey, o.o_totalprice, p.p_name, n.n_name
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
JOIN nation n ON c.c_nationkey = n.n_nationkey
WHERE o.o_totalprice > 50000;

-- 35. Grupare și agregare cu condiții
-- 35. Segmentele de piață active.
SELECT c.c_mktsegment, COUNT(c.c_custkey) AS num_customers, SUM(o.o_totalprice) AS total_sales
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_mktsegment
HAVING COUNT(c.c_custkey) > 10;

-- 36. Interogare cu subinterogare corelată
-- 36. Lista clienților care au plasat comenzi semnificative.
SELECT c.c_name FROM customer c
WHERE EXISTS (
    SELECT 1 FROM orders o WHERE o.o_custkey = c.c_custkey
    AND o.o_totalprice > (SELECT AVG(o_totalprice) FROM orders)
);

-- 37. Select cu UNION pentru combinarea clienților din două regiuni
-- 37. O listă de clienți din aceste două regiuni.
SELECT c.c_name, r.r_name FROM customer c
JOIN nation n ON c.c_nationkey = n.n_nationkey
JOIN region r ON n.n_regionkey = r.r_regionkey
WHERE r.r_name = 'AMERICA'
UNION
SELECT c.c_name, r.r_name FROM customer c
JOIN nation n ON c.c_nationkey = n.n_nationkey
JOIN region r ON n.n_regionkey = r.r_regionkey
WHERE r.r_name = 'EUROPE';

-- 38. Utilizarea DISTINCT ON pentru selectarea celei mai recente comenzi per client
-- 38. Lista comenzilor unice per client.
SELECT DISTINCT ON (o.o_custkey) o.o_custkey, o.o_orderdate, o.o_totalprice
FROM orders o
ORDER BY o.o_custkey, o.o_orderdate DESC;

-- 39. Interogare cu COUNT și CASE
-- 39. Numărul comenzilor pe categorii de valoare.
SELECT 
    CASE 
        WHEN o_totalprice > 50000 THEN 'High Value'
        WHEN o_totalprice > 10000 THEN 'Medium Value'
        ELSE 'Low Value'
    END AS order_category,
    COUNT(*) AS num_orders
FROM orders
GROUP BY order_category;

-- 40. Select cu ORDER BY și LIMIT pentru top 10 clienți după cheltuieli
-- 40. Lista celor mai buni clienți.
SELECT c.c_name, SUM(o.o_totalprice) AS total_spent
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
ORDER BY total_spent DESC
LIMIT 10;

-- 41. Self JOIN pentru identificarea clienților din aceeași țară
-- 41. Listează perechi de clienți din aceeași națiune.
SELECT a.c_name AS client1, b.c_name AS client2, a.c_nationkey
FROM customer a
JOIN customer b ON a.c_nationkey = b.c_nationkey AND a.c_custkey < b.c_custkey;

-- 42. Interogare cu LEFT JOIN și filtrare pe NULL
-- 42. Lista clienților fără comenzi.
SELECT c.c_name FROM customer c
LEFT JOIN orders o ON c.c_custkey = o.o_custkey
WHERE o.o_orderkey IS NULL;

-- 43. Select cu INTERSECT pentru clienți care au făcut și comenzi mici și mari
-- 43. Lista clienților care au făcut comenzi în ambele categorii.
SELECT c.c_name FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
WHERE o.o_totalprice > 40000
INTERSECT
SELECT c.c_name FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
WHERE o.o_totalprice < 8000;

-- 44. Interogare avansată cu fereastră și clasificare a clienților
-- 44. Clienții și rangul lor în națiunea lor.
SELECT c.c_name, n.n_name, SUM(o.o_totalprice) AS total_spent,
       RANK() OVER (PARTITION BY n.n_name ORDER BY SUM(o.o_totalprice) DESC) AS rank
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN nation n ON c.c_nationkey = n.n_nationkey
GROUP BY c.c_name, n.n_name
HAVING SUM(o.o_totalprice) > 5000;

-- 45. Grupare avansată cu ROLLUP pentru analiza vânzărilor
-- 45. Un rezumat al vânzărilor pe diferite niveluri geografice.
SELECT r.r_name, n.n_name, SUM(o.o_totalprice) AS total_sales
FROM region r
JOIN nation n ON r.r_regionkey = n.n_regionkey
JOIN customer c ON n.n_nationkey = c.c_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY ROLLUP(r.r_name, n.n_name);

-- 46. Interogare cu FILTER pentru calculul vânzărilor sezoniere
-- 46. Lista vânzărilor sezoniere.
SELECT EXTRACT(YEAR FROM o.o_orderdate) AS order_year,
       SUM(o.o_totalprice) FILTER (WHERE EXTRACT(MONTH FROM o.o_orderdate) IN (3,4,5)) AS spring_sales,
       SUM(o.o_totalprice) FILTER (WHERE EXTRACT(MONTH FROM o.o_orderdate) IN (6,7,8)) AS summer_sales,
       SUM(o.o_totalprice) FILTER (WHERE EXTRACT(MONTH FROM o.o_orderdate) IN (9,10,11)) AS autumn_sales,
       SUM(o.o_totalprice) FILTER (WHERE EXTRACT(MONTH FROM o.o_orderdate) IN (12,1,2)) AS winter_sales
FROM orders o
GROUP BY order_year;

-- 47. Utilizarea ARRAY_AGG pentru gruparea produselor comandate per client
-- 47. Lista de produse achiziționate de fiecare client.
SELECT c.c_name, ARRAY_AGG(DISTINCT p.p_name) AS purchased_products
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
GROUP BY c.c_name;

-- 48. Identificarea furnizorilor care aprovizionează cele mai multe piese
-- 48. Lista furnizorilor și numărul total de piese furnizate.
SELECT s.s_name, COUNT(ps.ps_partkey) AS total_supplied
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
GROUP BY s.s_name
ORDER BY total_supplied DESC LIMIT 10;

-- 53. Utilizarea WINDOW FUNCTION pentru calculul ratei de creștere a vânzărilor
-- 53. Lista anilor și rata de creștere.
SELECT order_year, total_sales,
       total_sales - LAG(total_sales) OVER (ORDER BY order_year) AS sales_growth
FROM (
    SELECT EXTRACT(YEAR FROM o.o_orderdate) AS order_year, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    GROUP BY order_year
) yearly_sales;

-- 54. Utilizarea SELF JOIN pentru identificarea clienților cu comenzi similare
-- Ce face: Găsește perechi de clienți care au plasat comenzi cu valori similare.
-- Ce returnează: Listează clienții cu valori de comandă apropiate.
SELECT c1.c_name AS customer1, c2.c_name AS customer2, ABS(o1.o_totalprice - o2.o_totalprice) AS price_difference
FROM orders o1
JOIN orders o2 ON o1.o_orderkey < o2.o_orderkey AND ABS(o1.o_totalprice - o2.o_totalprice) < 100
JOIN customer c1 ON o1.o_custkey = c1.c_custkey
JOIN customer c2 ON o2.o_custkey = c2.c_custkey;

-- 55. Select avansat cu fereastră pentru identificarea comenzilor sezoniere
-- 55. Lista comenzilor împărțite pe anotimpuri și clasificate în funcție de valoare.
SELECT o.o_orderkey, o.o_totalprice,
       CASE 
           WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (3,4,5) THEN 'Spring'
           WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (6,7,8) THEN 'Summer'
           WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (9,10,11) THEN 'Autumn'
           ELSE 'Winter'
       END AS season,
       RANK() OVER (PARTITION BY 
           CASE 
               WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (3,4,5) THEN 'Spring'
               WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (6,7,8) THEN 'Summer'
               WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (9,10,11) THEN 'Autumn'
               ELSE 'Winter'
           END ORDER BY o.o_totalprice DESC) AS season_rank
FROM orders o;

-- 56. Utilizarea UNION ALL pentru combinarea mai multor seturi de date
-- 56. Lista comenzilor segmentate pe categorii de valoare.
SELECT 'High Value' AS category, o.o_orderkey, o.o_totalprice
FROM orders o WHERE o.o_totalprice > 70000
UNION ALL
SELECT 'Low Value' AS category, o.o_orderkey, o.o_totalprice
FROM orders o WHERE o.o_totalprice < 8000;

-- 57. Calculul mediei ponderate a vânzărilor per client
-- 57. Lista clienților și media ponderată a comenzilor lor.
SELECT c.c_name, 
       SUM(o.o_totalprice * l.l_quantity) / SUM(l.l_quantity) AS weighted_avg_price
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY c.c_name;

-- 58. Analiza evoluției vânzărilor cu WINDOW FUNCTION
-- 58. Lista lunilor și creșterea procentuală a vânzărilor față de luna precedentă.
SELECT sales_month, total_sales,
       (total_sales - LAG(total_sales) OVER (ORDER BY sales_month)) / LAG(total_sales) OVER (ORDER BY sales_month) * 100 AS growth_rate
FROM (
    SELECT DATE_TRUNC('month', o.o_orderdate) AS sales_month, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    GROUP BY sales_month
) monthly_sales;

-- 59. Interogare avansată cu CTE pentru analiza profitabilității
-- 59. Lista produselor și marja lor de profit.
WITH ProductCosts AS (
    SELECT p.p_partkey, p.p_name, SUM(ps.ps_supplycost * l.l_quantity) AS total_cost,
           SUM(l.l_extendedprice) AS total_revenue
    FROM part p
    JOIN partsupp ps ON p.p_partkey = ps.ps_partkey
    JOIN lineitem l ON p.p_partkey = l.l_partkey
    GROUP BY p.p_partkey, p.p_name
)
SELECT p.p_name, total_revenue, total_cost, (total_revenue - total_cost) AS profit_margin
FROM ProductCosts p;

-- 60. Select avansat cu HAVING pentru determinarea furnizorilor dominanți
-- 60. Lista furnizorilor și numărul de clienți diferiți cărora le-au furnizat piese.
SELECT s.s_name, COUNT(DISTINCT o.o_custkey) AS unique_customers
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name
HAVING COUNT(DISTINCT o.o_custkey) >= 20;

-- 61. Utilizarea GROUPING SETS pentru analiza vânzărilor
-- 61. Totalul vânzărilor pentru fiecare regiune, națiune și total general.
SELECT r.r_name, n.n_name, SUM(o.o_totalprice) AS total_sales
FROM region r
JOIN nation n ON r.r_regionkey = n.n_regionkey
JOIN customer c ON n.n_nationkey = c.c_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY GROUPING SETS ((r.r_name, n.n_name), (r.r_name), ());

-- 62. SELF JOIN avansat pentru identificarea clienților cu comportament similar
-- 62. Lista perechilor de clienți cu diferențe mici între sumele totale cheltuite.
SELECT c1.c_name AS customer1, c2.c_name AS customer2,
       ABS(SUM(o1.o_totalprice) - SUM(o2.o_totalprice)) AS spending_difference
FROM customer c1
JOIN orders o1 ON c1.c_custkey = o1.o_custkey
JOIN customer c2 ON c1.c_custkey <> c2.c_custkey
JOIN orders o2 ON c2.c_custkey = o2.o_custkey
GROUP BY c1.c_name, c2.c_name
HAVING ABS(SUM(o1.o_totalprice) - SUM(o2.o_totalprice)) < 5000;

-- 63. Crearea unui VIEW pentru analiza comenzilor de mare valoare
-- 63. O vedere a comenzilor mari.
CREATE VIEW high_value_orders AS
SELECT * FROM orders WHERE o_totalprice > 50000;

-- 64. Calculul coeficientului de variație pentru valoarea comenzilor
-- 64 Lista clienților cu variația comenzilor lor.
SELECT c.c_name, 
       STDDEV(o.o_totalprice) / AVG(o.o_totalprice) AS variation_coefficient
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name;

-- 65. Interogare cu ROLLUP și HAVING pentru analiza pieței
-- 65. Lista segmentelor de piață și totalul vânzărilor.
SELECT c.c_mktsegment, n.n_name, SUM(o.o_totalprice) AS total_sales
FROM customer c
JOIN nation n ON c.c_nationkey = n.n_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY ROLLUP(c.c_mktsegment, n.n_name)
HAVING SUM(o.o_totalprice) > 100000;

-- 66. Interogare avansată cu LATERAL JOIN
-- 66. Lista comenzilor și cel mai scump produs comandat.
SELECT o.o_orderkey, max_price.p_name, max_price.max_price
FROM orders o
JOIN LATERAL (
    SELECT l.l_orderkey, p.p_name, MAX(l.l_extendedprice) AS max_price
    FROM lineitem l
    JOIN part p ON l.l_partkey = p.p_partkey
    WHERE l.l_orderkey = o.o_orderkey
    GROUP BY l.l_orderkey, p.p_name
) max_price ON true;

-- 67. Utilizarea ARRAY FUNCTION pentru analiza comenzilor
-- 67. Lista clienților și comenzile lor sub formă de array.
SELECT c.c_name, ARRAY_AGG(o.o_orderkey) AS order_ids
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name;

-- 68. Calcularea ponderii unui produs în totalul comenzilor
-- 68. Lista produselor și ponderea lor în totalul vânzărilor.
SELECT p.p_name, 
       SUM(l.l_extendedprice) AS total_sales,
       SUM(l.l_extendedprice) * 100 / (SELECT SUM(l_extendedprice) FROM lineitem) AS percentage_of_total
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name;

-- 69. Utilizarea COMMON TABLE EXPRESSIONS (CTE) pentru calculul duratei medii dintre comen
-- 69. Lista clienților și media zilelor dintre comenzi.
WITH OrderDurations AS (
    SELECT o.o_custkey, 
           o.o_orderdate, 
           LAG(o.o_orderdate) OVER (PARTITION BY o.o_custkey ORDER BY o.o_orderdate) AS prev_order_date
    FROM orders o
)
SELECT c.c_name, AVG(o.o_orderdate - o.prev_order_date) AS avg_days_between_orders
FROM OrderDurations o
JOIN customer c ON o.o_custkey = c.c_custkey
WHERE o.prev_order_date IS NOT NULL
GROUP BY c.c_name;

-- 70. SELF JOIN pentru identificarea comenzilor similare în funcție de produse
-- 70. Lista perechilor de comenzi cu cel puțin 80% din produse identice.
SELECT o1.o_orderkey AS order1, o2.o_orderkey AS order2, 
       COUNT(*) * 100.0 / (SELECT COUNT(*) FROM lineitem WHERE l_orderkey = o1.o_orderkey) AS similarity_percentage
FROM lineitem l1
JOIN lineitem l2 ON l1.l_partkey = l2.l_partkey
JOIN orders o1 ON l1.l_orderkey = o1.o_orderkey
JOIN orders o2 ON l2.l_orderkey = o2.o_orderkey AND o1.o_orderkey < o2.o_orderkey
GROUP BY o1.o_orderkey, o2.o_orderkey
HAVING COUNT(*) * 100.0 / (SELECT COUNT(*) FROM lineitem WHERE l_orderkey = o1.o_orderkey) > 80;

-- 71. Identificarea produselor vândute doar în anumite țări
-- 71. Lista produselor și țările unde au fost vândute.
SELECT p.p_name, n.n_name
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
JOIN customer c ON o.o_custkey = c.c_custkey
JOIN nation n ON c.c_nationkey = n.n_nationkey
GROUP BY p.p_name, n.n_name
HAVING COUNT(DISTINCT n.n_name) = 1;

-- 72. Analiza profitabilității comenzilor cu subinterogări
-- 72. Lista comenzilor și profitul obținut.
SELECT o.o_orderkey, 
       SUM(l.l_extendedprice) AS revenue, 
       SUM(ps.ps_supplycost * l.l_quantity) AS total_cost, 
       SUM(l.l_extendedprice - (ps.ps_supplycost * l.l_quantity)) AS profit
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN partsupp ps ON l.l_partkey = ps.ps_partkey AND l.l_suppkey = ps.ps_suppkey
GROUP BY o.o_orderkey;

-- 73. Calcularea creșterii anuale a numărului de clienți
-- 73. Lista anilor și creșterea procentuală.
SELECT order_year, total_customers,
       (total_customers - LAG(total_customers) OVER (ORDER BY order_year)) * 100.0 / 
       LAG(total_customers) OVER (ORDER BY order_year) AS growth_rate
FROM (
    SELECT EXTRACT(YEAR FROM o.o_orderdate) AS order_year, COUNT(DISTINCT o.o_custkey) AS total_customers
    FROM orders o
    GROUP BY order_year
) yearly_customers;

-- 74. Detectarea furnizorilor care nu mai sunt activi
-- 74. Lista furnizorilor inactivi.
SELECT s.s_name
FROM supplier s
LEFT JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
LEFT JOIN lineitem l ON ps.ps_partkey = l.l_partkey AND ps.ps_suppkey = l.l_suppkey
LEFT JOIN orders o ON l.l_orderkey = o.o_orderkey
WHERE o.o_orderdate IS NULL OR o.o_orderdate < CURRENT_DATE - INTERVAL '1 year';

-- 75. Găsirea clienților care și-au schimbat tiparul de cumpărături
-- 75. Lista clienților cu variații mari în obiceiurile de cumpărare.
WITH customer_orders AS (
    SELECT c.c_custkey, c.c_name, 
           EXTRACT(YEAR FROM o.o_orderdate) AS order_year,
           COUNT(o.o_orderkey) AS total_orders
    FROM customer c
    JOIN orders o ON c.c_custkey = o.o_custkey
    GROUP BY c.c_custkey, c.c_name, order_year
)
SELECT c1.c_name, c1.total_orders AS current_year_orders, c2.total_orders AS previous_year_orders,
       (c1.total_orders - c2.total_orders) * 100.0 / c2.total_orders AS change_percentage
FROM customer_orders c1
LEFT JOIN customer_orders c2 ON c1.c_custkey = c2.c_custkey AND c1.order_year = c2.order_year + 1
WHERE c2.total_orders IS NOT NULL AND ABS((c1.total_orders - c2.total_orders) * 100.0 / c2.total_orders) > 30;

-- 76. Interogare cu GROUPING SETS pentru analiza segmentelor de piață
-- 76. Vânzările per segment, națiune și total global.
SELECT c.c_mktsegment, n.n_name, SUM(o.o_totalprice) AS total_sales
FROM customer c
JOIN nation n ON c.c_nationkey = n.n_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY GROUPING SETS ((c.c_mktsegment, n.n_name), (c.c_mktsegment), ());

-- 77. Identificarea celor mai cumpărate produse pe segmente de piață
SELECT c.c_mktsegment, p.p_name, SUM(l.l_quantity) AS total_sold
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
GROUP BY c.c_mktsegment, p.p_name
ORDER BY c.c_mktsegment, total_sold DESC;

-- 78. Interogare folosind LATERAL JOIN pentru cea mai mare comandă per client
-- 78. Cea mai scumpă comandă a fiecărui client.
SELECT c.c_name, highest_order.*
FROM customer c
JOIN LATERAL (
    SELECT o.o_orderkey, o.o_totalprice
    FROM orders o
    WHERE o.o_custkey = c.c_custkey
    ORDER BY o.o_totalprice DESC
    LIMIT 1
) highest_order ON true;

-- 79. O listă de regiuni cu venitul și procentul de creștere.
SELECT n.n_name AS nation, sales.order_year, sales.total_sales,
       LAG(sales.total_sales) OVER (PARTITION BY n.n_name ORDER BY sales.order_year) AS prev_year_sales,
       (sales.total_sales - LAG(sales.total_sales) OVER (PARTITION BY n.n_name ORDER BY sales.order_year)) 
       / LAG(sales.total_sales) OVER (PARTITION BY n.n_name ORDER BY sales.order_year) * 100 AS growth_percentage
FROM (
    SELECT c.c_nationkey, EXTRACT(YEAR FROM o.o_orderdate) AS order_year, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    JOIN customer c ON o.o_custkey = c.c_custkey
    GROUP BY c.c_nationkey, order_year
) sales
JOIN nation n ON sales.c_nationkey = n.n_nationkey;

-- 80. Identificați clienții fără comenzi recente
SELECT c.c_name, MAX(o.o_orderdate) AS last_order_date
FROM customer c
LEFT JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
HAVING MAX(o.o_orderdate) < CURRENT_DATE - INTERVAL '1 year';

-- 81. Identificați cele mai comandate produse pe lună
SELECT sales_month, p.p_name, total_quantity
FROM (
    SELECT DATE_TRUNC('month', o.o_orderdate) AS sales_month, l.l_partkey,
           SUM(l.l_quantity) AS total_quantity,
           RANK() OVER (PARTITION BY DATE_TRUNC('month', o.o_orderdate) ORDER BY SUM(l.l_quantity) DESC) AS rank
    FROM lineitem l
    JOIN orders o ON l.l_orderkey = o.o_orderkey
    GROUP BY sales_month, l.l_partkey
) ranked_products
JOIN part p ON ranked_products.l_partkey = p.p_partkey
WHERE rank = 1;

-- 82. Analizați contribuția la venituri a fiecărei categorii de clienți
SELECT c.c_name, 
       CASE 
           WHEN SUM(o.o_totalprice) > 100000 THEN 'High Value'
           WHEN SUM(o.o_totalprice) BETWEEN 50000 AND 100000 THEN 'Medium Value'
           ELSE 'Low Value'
       END AS customer_category,
       SUM(o.o_totalprice) AS total_spent
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name;

-- 83. Identificați clienții care au comandat aceleași produse
SELECT DISTINCT c1.c_name AS customer1, c2.c_name AS customer2, l1.l_partkey
FROM orders o1
JOIN lineitem l1 ON o1.o_orderkey = l1.l_orderkey
JOIN customer c1 ON o1.o_custkey = c1.c_custkey
JOIN orders o2 ON o1.o_orderkey <> o2.o_orderkey
JOIN lineitem l2 ON o2.o_orderkey = l2.l_orderkey AND l1.l_partkey = l2.l_partkey
JOIN customer c2 ON o2.o_custkey = c2.c_custkey;

-- 84. Calculați media mobilă a vânzărilor în ultimele 3 luni
SELECT sales_month, total_sales,
       AVG(total_sales) OVER (ORDER BY sales_month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg
FROM (
    SELECT DATE_TRUNC('month', o.o_orderdate) AS sales_month, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    GROUP BY sales_month
) monthly_sales;


-- 85. Identificați cei mai profitabili furnizori
SELECT s.s_name, SUM(l.l_extendedprice) - SUM(ps.ps_supplycost * l.l_quantity) AS total_profit
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
GROUP BY s.s_name
ORDER BY total_profit DESC LIMIT 10;

-- 86. Identificarea produselor cu vânzări fluctuante
WITH MonthlySales AS (
    SELECT p.p_partkey, p.p_name, 
           DATE_TRUNC('month', o.o_orderdate) AS sales_month,
           SUM(l.l_extendedprice) AS total_sales
    FROM part p
    JOIN lineitem l ON p.p_partkey = l.l_partkey
    JOIN orders o ON l.l_orderkey = o.o_orderkey
    GROUP BY p.p_partkey, p.p_name, sales_month
)
SELECT p_name, sales_month, total_sales,
       (total_sales - LAG(total_sales) OVER (PARTITION BY p_partkey ORDER BY sales_month)) / 
       NULLIF(LAG(total_sales) OVER (PARTITION BY p_partkey ORDER BY sales_month), 0) * 100 AS variation_percentage
FROM MonthlySales;

--87. Analiza furnizorilor cu cele mai multe produse livrate
SELECT s.s_name, COUNT(DISTINCT ps.ps_partkey) AS unique_products
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
GROUP BY s.s_name
ORDER BY unique_products DESC
LIMIT 10;

-- 88. Determinarea sezonalității comenzilor
SELECT 
    CASE 
        WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (3,4,5) THEN 'Spring'
        WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (6,7,8) THEN 'Summer'
        WHEN EXTRACT(MONTH FROM o.o_orderdate) IN (9,10,11) THEN 'Autumn'
        ELSE 'Winter'
    END AS season,
    COUNT(*) AS total_orders
FROM orders o
GROUP BY season
ORDER BY total_orders DESC;

-- 89. Clasificarea produselor pe baza prețului lor mediu
SELECT p.p_name, p.p_retailprice,
       CASE 
           WHEN p.p_retailprice > 1000 THEN 'Expensive'
           WHEN p.p_retailprice > 500 THEN 'Medium'
           ELSE 'Cheap'
       END AS price_category
FROM part p;

-- 90. Calcularea timpului mediu de livrare
SELECT AVG(l.l_shipdate - o.o_orderdate) AS avg_delivery_days
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey;

-- 91. Găsirea furnizorilor cu cel mai mare cost mediu de aprovizionare
SELECT s.s_name, AVG(ps.ps_supplycost) AS avg_supply_cost
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
GROUP BY s.s_name
ORDER BY avg_supply_cost DESC
LIMIT 10;

-- 92. Analiza produselor cu cel mai mare impact asupra vânzărilor
SELECT p.p_name, 
       SUM(l.l_extendedprice) AS total_sales,
       SUM(l.l_extendedprice) * 100 / (SELECT SUM(l_extendedprice) FROM lineitem) AS sales_percentage
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY total_sales DESC
LIMIT 10;

-- 93. Compararea vânzărilor anuale pentru identificarea creșterii
 WITH AnnualSales AS (
    SELECT EXTRACT(YEAR FROM o.o_orderdate) AS order_year, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    GROUP BY order_year
)
SELECT order_year, total_sales,
       (total_sales - LAG(total_sales) OVER (ORDER BY order_year)) / NULLIF(LAG(total_sales) OVER (ORDER BY order_year), 0) * 100 AS growth_rate
FROM AnnualSales;

-- 94. Determinarea clienților care au cheltuit cel mai mult.
SELECT c.c_name, SUM(o.o_totalprice) AS total_spent
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
ORDER BY total_spent DESC
LIMIT 10;

-- 95. Calcularea vânzărilor medii mobile pe 3 luni
SELECT sales_month, total_sales,
       AVG(total_sales) OVER (ORDER BY sales_month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg
FROM (
    SELECT DATE_TRUNC('month', o.o_orderdate) AS sales_month, SUM(o.o_totalprice) AS total_sales
    FROM orders o
    GROUP BY sales_month
) monthly_sales;

-- 96. Analiza produselor care au fost returnate cel mai des
SELECT p.p_name, COUNT(*) AS return_count
FROM lineitem l
JOIN part p ON l.l_partkey = p.p_partkey
WHERE l.l_returnflag = 'R'
GROUP BY p.p_name
ORDER BY return_count DESC
LIMIT 10;

-- 97. Determinarea furnizorilor care oferă cele mai ieftine produse
SELECT s.s_name, p.p_name, ps.ps_supplycost
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN part p ON ps.ps_partkey = p.p_partkey
ORDER BY ps.ps_supplycost ASC
LIMIT 10;

-- 98. Lista furnizorilor și procentul de creștere/scădere a comenzilor în timp
WITH SupplierOrders AS (
    SELECT s.s_name, EXTRACT(YEAR FROM o.o_orderdate) AS order_year, 
           COUNT(DISTINCT o.o_orderkey) AS total_orders
    FROM supplier s
    JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
    JOIN lineitem l ON ps.ps_partkey = l.l_partkey
    JOIN orders o ON l.l_orderkey = o.o_orderkey
    GROUP BY s.s_name, order_year
)
SELECT s_name, order_year, total_orders,
       (total_orders - LAG(total_orders) OVER (PARTITION BY s_name ORDER BY order_year)) / 
       NULLIF(LAG(total_orders) OVER (PARTITION BY s_name ORDER BY order_year), 0) * 100 AS growth_rate
FROM SupplierOrders;


-- 99. Lista produselor care au fost vândute în toate regiunile.
    SELECT l.l_partkey, r.r_name
    FROM lineitem l
    JOIN orders o ON l.l_orderkey = o.o_orderkey
    JOIN customer c ON o.o_custkey = c.c_custkey
    JOIN nation n ON c.c_nationkey = n.n_nationkey
    JOIN region r ON n.n_regionkey = r.r_regionkey
    GROUP BY l.l_partkey, r.r_name
)
SELECT p.p_name
FROM part p
JOIN ProductRegions pr ON p.p_partkey = pr.l_partkey
GROUP BY p.p_name
HAVING COUNT(DISTINCT pr.r_name) = (SELECT COUNT(*) FROM region);

-- 100. Lista primelor 10 produse, prețul minim, maxim și diferența dintre acestea.
WITH ProductPriceStats AS (
    SELECT p.p_name, 
           MIN(l.l_extendedprice / l.l_quantity) AS min_price,
           MAX(l.l_extendedprice / l.l_quantity) AS max_price,
           MAX(l.l_extendedprice / l.l_quantity) - MIN(l.l_extendedprice / l.l_quantity) AS price_variation
    FROM part p
    JOIN lineitem l ON p.p_partkey = l.l_partkey
    GROUP BY p.p_name
)
SELECT p_name, min_price, max_price, price_variation
FROM ProductPriceStats
ORDER BY price_variation DESC
LIMIT 10;

-- 101. Produsele vândute de cel puțin 4 furnizori diferiți
SELECT 
    p.p_name, 
    COUNT(DISTINCT s.s_name) AS num_suppliers 
FROM part p 
JOIN partsupp ps ON p.p_partkey = ps.ps_partkey 
JOIN supplier s ON ps.ps_suppkey = s.s_suppkey 
GROUP BY p.p_name 
HAVING COUNT(DISTINCT s.s_name) >= 4;

-- 102. Top 5 clienți cu cele mai mari cheltuieli
SELECT 
    c.c_name, 
    SUM(o.o_totalprice) AS total_spent 
FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
GROUP BY c.c_name 
ORDER BY total_spent DESC 
LIMIT 5;

-- 103. Comenzile cu valoare peste media totală
SELECT 
    o.o_orderkey, 
    o.o_totalprice 
FROM orders o 
WHERE o.o_totalprice > (SELECT AVG(o_totalprice) FROM orders);

-- 104. Media prețului per produs în fiecare națiune
SELECT 
    n.n_name, 
    AVG(p.p_retailprice) 
FROM nation n 
JOIN supplier s ON n.n_nationkey = s.s_nationkey 
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey 
JOIN part p ON ps.ps_partkey = p.p_partkey 
GROUP BY n.n_name;

-- 105. Comenzile livrate cu întârziere
SELECT 
    o.o_orderkey, 
    l.l_shipdate, 
    o.o_orderdate 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
WHERE l.l_shipdate > o.o_orderdate;

-- 106. Furnizorii cu cel mai mare sold
SELECT 
    s.s_name, 
    s.s_acctbal 
FROM supplier s 
WHERE s.s_acctbal = (SELECT MAX(s_acctbal) FROM supplier);

-- 107. Găsește comenzile plasate de clienți dintr-o națiune specifică
SELECT 
    o.o_orderkey, 
    c.c_name 
FROM orders o 
JOIN customer c ON o.o_custkey = c.c_custkey 
WHERE c.c_nationkey IN (SELECT n.n_nationkey FROM nation n WHERE n.n_name = 'FRANCE');

-- 108. Comenzile care au discount mai mare de 10%
SELECT 
    o.o_orderkey, 
    l.l_discount 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
WHERE l.l_discount >= 0.10;

-- 109. Comenzile care conțin cel puțin 4 produse diferite
SELECT 
    o.o_orderkey, 
    COUNT(DISTINCT l.l_partkey) AS num_products 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
GROUP BY o.o_orderkey 
HAVING COUNT(DISTINCT l.l_partkey) >= 4;

-- 110. Furnizorii care au livrat produse mai scumpe decât media
SELECT 
    s.s_name, 
    ps.ps_supplycost 
FROM supplier s 
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey 
WHERE ps.ps_supplycost > (SELECT AVG(ps_supplycost) FROM partsupp);

-- 111. Găsește cele mai profitabile 10 produse
SELECT 
    p.p_name, 
    SUM(l.l_extendedprice - (l.l_quantity * ps.ps_supplycost)) AS total_profit 
FROM part p 
JOIN partsupp ps ON p.p_partkey = ps.ps_partkey 
JOIN lineitem l ON ps.ps_partkey = l.l_partkey 
GROUP BY p.p_name 
ORDER BY total_profit DESC 
LIMIT 10;

-- 112. Cele mai mari 5 comenzi în fiecare lună
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) || '-' || EXTRACT(MONTH FROM o.o_orderdate) AS order_month, 
    o.o_orderkey, 
    o.o_totalprice 
FROM orders o 
WHERE o.o_totalprice IN (
    SELECT o2.o_totalprice FROM orders o2 
    WHERE EXTRACT(YEAR FROM o.o_orderdate) || '-' || EXTRACT(MONTH FROM o.o_orderdate) = EXTRACT(YEAR FROM o.o_orderdate) || '-' || EXTRACT(MONTH FROM o.o_orderdate) 
    ORDER BY o2.o_totalprice DESC 
    LIMIT 5
);

-- 113. Națiunile cu cei mai mulți furnizori
SELECT 
    n.n_name, 
    COUNT(s.s_suppkey) AS total_suppliers 
FROM nation n 
JOIN supplier s ON n.n_nationkey = s.s_nationkey 
GROUP BY n.n_name 
ORDER BY total_suppliers DESC;

-- 114. Găsește clienții cu comenzile cele mai mari per segment de piață
SELECT 
    c.c_mktsegment, 
    c.c_name, 
    MAX(o.o_totalprice) AS max_order 
FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
GROUP BY c.c_mktsegment, c.c_name;

-- 115. Comenzile livrate cu mai mult de 2 săptămâni întârziere
SELECT 
    o.o_orderkey, 
    l.l_shipdate, 
    o.o_orderdate 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
WHERE CAST(l.l_shipdate AS DATE) - CAST(o.o_orderdate AS DATE) > 14;

-- 116. Cele mai scumpe comenzi per client
SELECT 
    c.c_name, 
    MAX(o.o_totalprice) AS max_order_value 
FROM customer c 
JOIN orders o ON c.c_custkey = o.o_custkey 
GROUP BY c.c_name;

-- 117. Top 3 clienți din fiecare națiune cu cele mai multe comenzi
WITH RankedCustomers AS (
    SELECT 
        n.n_name, 
        c.c_name, 
        COUNT(o.o_orderkey) AS total_orders,
        RANK() OVER (PARTITION BY n.n_name ORDER BY COUNT(o.o_orderkey) DESC) AS rnk
    FROM nation n 
    JOIN customer c ON n.n_nationkey = c.c_nationkey 
    JOIN orders o ON c.c_custkey = o.o_custkey 
    GROUP BY n.n_name, c.c_name
)
SELECT n_name, c_name, total_orders 
FROM RankedCustomers 
WHERE rnk <= 3
ORDER BY n_name, total_orders DESC;

-- 118. Top 10 furnizori cu cel mai mare volum de vânzări
SELECT 
    s.s_name, 
    SUM(l.l_extendedprice) AS total_sales 
FROM supplier s 
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey 
JOIN lineitem l ON ps.ps_partkey = l.l_partkey 
GROUP BY s.s_name 
ORDER BY total_sales DESC 
LIMIT 10;

-- 119. Comenzile cu cele mai mari reduceri per lună
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) || '-' || EXTRACT(MONTH FROM o.o_orderdate) AS order_month, 
    o.o_orderkey, 
    MAX(l.l_discount) AS max_discount 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
GROUP BY order_month, o.o_orderkey 
ORDER BY order_month, max_discount DESC;

-- 120. Comenzile cu un număr maxim de produse unice
SELECT 
    o.o_orderkey, 
    COUNT(DISTINCT l.l_partkey) AS unique_products 
FROM orders o 
JOIN lineitem l ON o.o_orderkey = l.l_orderkey 
GROUP BY o.o_orderkey 
ORDER BY unique_products DESC 
LIMIT 1;

-- 121. Cele mai frecvente combinații de produse comandate împreună
SELECT 
    l1.l_partkey AS product_1, 
    l2.l_partkey AS product_2, 
    COUNT(*) AS total_orders
FROM lineitem l1
JOIN lineitem l2 ON l1.l_orderkey = l2.l_orderkey AND l1.l_partkey < l2.l_partkey
GROUP BY l1.l_partkey, l2.l_partkey
ORDER BY total_orders DESC
LIMIT 10;

-- 122. Produsele care au avut cel mai mare preț vândut în fiecare lună
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    p.p_name,
    MAX(l.l_extendedprice) AS max_price
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
GROUP BY year, month, p.p_name
ORDER BY year DESC, month DESC, max_price DESC;

-- 123. Cele mai comandate 10 produse în ultimele 3 luni
SELECT 
    p.p_name, 
    COUNT(l.l_partkey) AS total_orders
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
WHERE o.o_orderdate >= (SELECT MAX(o_orderdate) - INTERVAL '3 MONTH' FROM orders)
GROUP BY p.p_name
ORDER BY total_orders DESC
LIMIT 10;

-- 124. Națiunile care au cumpărat cele mai scumpe produse
SELECT 
    n.n_name, 
    MAX(l.l_extendedprice) AS max_purchase
FROM nation n
JOIN customer c ON n.n_nationkey = c.c_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY n.n_name
ORDER BY max_purchase DESC;

-- 125. Clienți care nu au mai plasat nicio comandă în ultimele 6 luni
SELECT 
    c.c_name
FROM customer c
WHERE c.c_custkey NOT IN (SELECT DISTINCT o.o_custkey FROM orders o WHERE o.o_orderdate >= (SELECT MAX(o_orderdate) - INTERVAL '6 MONTH' FROM orders));

-- 126. Cele mai lungi termene de livrare per produs
SELECT 
    p.p_name, 
    MAX(l.l_shipdate - l.l_commitdate) AS max_delivery_time
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY max_delivery_time DESC;

-- 127. Cele mai mari comenzi plasate în fiecare țară
SELECT 
    n.n_name, 
    MAX(o.o_totalprice) AS max_order
FROM nation n
JOIN customer c ON n.n_nationkey = c.c_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY n.n_name
ORDER BY max_order DESC;

-- 128. Clienții care au plasat comenzi pentru cel puțin 5 produse diferite
SELECT 
    c.c_name, 
    COUNT(DISTINCT l.l_partkey) AS unique_products
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT l.l_partkey) >= 5;

-- 129. Cele mai mari 5 comenzi per lună
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    o.o_orderkey, 
    o.o_totalprice
FROM orders o
WHERE o.o_totalprice IN (
    SELECT o2.o_totalprice FROM orders o2
    WHERE EXTRACT(YEAR FROM o2.o_orderdate) = EXTRACT(YEAR FROM o.o_orderdate)
    AND EXTRACT(MONTH FROM o2.o_orderdate) = EXTRACT(MONTH FROM o.o_orderdate)
    ORDER BY o2.o_totalprice DESC
    LIMIT 5
);

-- 130. Clienții care au returnat cel mai mare număr de produse
SELECT 
    c.c_name, 
    COUNT(l.l_returnflag) AS returns
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
WHERE l.l_returnflag = 'R'
GROUP BY c.c_name
ORDER BY returns DESC;

-- 131. Cele mai vândute produse per furnizor
SELECT 
    s.s_name, 
    p.p_name, 
    COUNT(l.l_partkey) AS total_sales
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN part p ON ps.ps_partkey = p.p_partkey
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY s.s_name, p.p_name
ORDER BY s.s_name, total_sales DESC;

-- 132. Cele mai scumpe comenzi livrate de fiecare furnizor
SELECT 
    s.s_name, 
    MAX(l.l_extendedprice) AS max_order_value
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
GROUP BY s.s_name
ORDER BY max_order_value DESC;

-- 133. Clienții care au comandat produse din cel puțin 10 țări diferite
SELECT 
    c.c_name, 
    COUNT(DISTINCT n.n_name) AS countries
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN supplier s ON l.l_suppkey = s.s_suppkey
JOIN nation n ON s.s_nationkey = n.n_nationkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT n.n_name) >= 10;

-- 134. Produsele cu cel mai mic cost de furnizare per furnizor
SELECT 
    s.s_name, 
    p.p_name, 
    MIN(ps.ps_supplycost) AS min_cost
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN part p ON ps.ps_partkey = p.p_partkey
GROUP BY s.s_name, p.p_name
ORDER BY s.s_name, min_cost ASC;

-- 135. Cele mai rapide comenzi livrate
SELECT 
    o.o_orderkey, 
    MIN(l.l_shipdate - o.o_orderdate) AS fastest_delivery_time
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY o.o_orderkey
ORDER BY fastest_delivery_time ASC;

-- 136. Cele mai frecvente metode de expediere utilizate
SELECT 
    l.l_shipmode, 
    COUNT(*) AS usage_count
FROM lineitem l
GROUP BY l.l_shipmode
ORDER BY usage_count DESC;

-- 137. Clienții care au plasat comenzi în fiecare an din ultimii 5 ani
SELECT 
    c.c_name
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT EXTRACT(YEAR FROM o.o_orderdate)) = 5;

-- 138. Produsele care au avut cel mai mare număr de comenzi unice
SELECT 
    p.p_name, 
    COUNT(DISTINCT l.l_orderkey) AS unique_orders
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY unique_orders DESC;

-- 139. Cele mai mari vânzări lunare per produs
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    p.p_name,
    SUM(l.l_extendedprice) AS total_sales
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
GROUP BY year, month, p.p_name
ORDER BY year DESC, month DESC, total_sales DESC;

-- 140. Cele mai profitabile 10 comenzi
SELECT 
    o.o_orderkey, 
    SUM(l.l_extendedprice - (l.l_quantity * ps.ps_supplycost)) AS total_profit
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN partsupp ps ON l.l_partkey = ps.ps_partkey
GROUP BY o.o_orderkey
ORDER BY total_profit DESC
LIMIT 10;

-- 141. Națiunile cu cei mai mulți clienți activi
SELECT 
    n.n_name, 
    COUNT(DISTINCT c.c_custkey) AS total_customers
FROM nation n
JOIN customer c ON n.n_nationkey = c.c_nationkey
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY n.n_name
ORDER BY total_customers DESC;

-- 142. Cele mai populare combinații de produse comandate împreună
SELECT 
    l1.l_partkey AS product_1, 
    l2.l_partkey AS product_2, 
    COUNT(*) AS total_orders
FROM lineitem l1
JOIN lineitem l2 ON l1.l_orderkey = l2.l_orderkey AND l1.l_partkey < l2.l_partkey
GROUP BY l1.l_partkey, l2.l_partkey
ORDER BY total_orders DESC
LIMIT 10;

-- 143. Cele mai mari 5 reduceri per lună
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    MAX(l.l_discount) AS max_discount
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY year, month
ORDER BY year DESC, month DESC, max_discount DESC;

-- 144. Cele mai frecvente motive de returnare
SELECT 
    l.l_returnflag, 
    COUNT(*) AS total_returns
FROM lineitem l
WHERE l.l_returnflag IS NOT NULL
GROUP BY l.l_returnflag
ORDER BY total_returns DESC;

-- 145. Cele mai mari costuri de livrare per furnizor
SELECT 
    s.s_name, 
    MAX(l.l_extendedprice - (l.l_quantity * ps.ps_supplycost)) AS max_shipping_cost
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
GROUP BY s.s_name
ORDER BY max_shipping_cost DESC;

-- 146. Cele mai mari comenzi per client în fiecare an
SELECT 
    c.c_name, 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    MAX(o.o_totalprice) AS max_order_value
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name, year
ORDER BY year DESC, max_order_value DESC;

-- 147. Cele mai mari profituri per produs
SELECT 
    p.p_name, 
    SUM(l.l_extendedprice - (l.l_quantity * ps.ps_supplycost)) AS total_profit
FROM part p
JOIN partsupp ps ON p.p_partkey = ps.ps_partkey
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY total_profit DESC;

-- 148. Cele mai mari comenzi plasate în ultimele 3 luni
SELECT 
    o.o_orderkey, 
    o.o_totalprice
FROM orders o
WHERE o.o_orderdate >= (SELECT MAX(o2.o_orderdate) - INTERVAL '3 MONTH' FROM orders o2)
ORDER BY o.o_totalprice DESC;

-- 149. Produsele care au fost vândute în cel mai mare număr de țări
SELECT 
    p.p_name, 
    COUNT(DISTINCT n.n_name) AS total_countries
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN supplier s ON l.l_suppkey = s.s_suppkey
JOIN nation n ON s.s_nationkey = n.n_nationkey
GROUP BY p.p_name
ORDER BY total_countries DESC;

-- 150. Cele mai lungi întârzieri de livrare per produs
SELECT 
    p.p_name, 
    MAX(l.l_shipdate - l.l_commitdate) AS max_delay
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY max_delay DESC;

-- 151. Cele mai frecvente priorități utilizate
SELECT 
    o.o_orderpriority, 
    COUNT(*) AS usage_count
FROM orders o
GROUP BY o.o_orderpriority
ORDER BY usage_count DESC;

-- 152. Clienți cu schimbări în frecvența comenzilor
SELECT 
    c.c_name, 
    o.o_orderdate, 
    LAG(o.o_orderdate) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS prev_order,
    o.o_orderdate - LAG(o.o_orderdate) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS days_between
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey;

-- 153. Comenzi care au o valoare mai mare decât media lunară a comenzilor
SELECT 
    o.o_orderkey, 
    o.o_totalprice,
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month
FROM orders o
WHERE o.o_totalprice > (
    SELECT AVG(o2.o_totalprice) 
    FROM orders o2
    WHERE EXTRACT(YEAR FROM o2.o_orderdate) = EXTRACT(YEAR FROM o.o_orderdate) 
    AND EXTRACT(MONTH FROM o2.o_orderdate) = EXTRACT(MONTH FROM o.o_orderdate)
);

-- 154. Identificarea comenzilor cu un model fluctuant de reduceri
SELECT 
    o.o_orderkey,
    l.l_discount,
    LAG(l.l_discount) OVER (PARTITION BY o.o_orderkey ORDER BY l.l_discount) AS prev_discount,
    CASE 
        WHEN l.l_discount > LAG(l.l_discount) OVER (PARTITION BY o.o_orderkey ORDER BY l.l_discount) THEN 'Discount Increased'
        WHEN l.l_discount < LAG(l.l_discount) OVER (PARTITION BY o.o_orderkey ORDER BY l.l_discount) THEN 'Discount Decreased'
        ELSE 'No Change'
    END AS discount_trend
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey;

-- 155. Determinarea celor mai consistente furnizări
SELECT 
    s.s_name, 
    STDDEV(l.l_quantity) AS supply_variation
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
GROUP BY s.s_name
ORDER BY supply_variation ASC;

-- 156. Clienți care au trecut de la comenzi mici la comenzi mari
SELECT 
    c.c_name, 
    o.o_totalprice,
    LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS prev_order_value,
    CASE 
        WHEN o.o_totalprice > LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) * 1.5 THEN 'Significant Increase'
        ELSE 'Stable'
    END AS order_trend
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey;

-- 157. Identificarea furnizorilor care și-au pierdut din volum
SELECT 
    s.s_name,
    SUM(l.l_quantity) AS current_volume,
    LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) AS prev_volume,
    CASE 
        WHEN SUM(l.l_quantity) < LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) THEN 'Volume Decreased'
        ELSE 'Stable'
    END AS supply_trend
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name, s.s_suppkey, o.o_orderdate;

-- 158. Produse care au avut cele mai mari variații de preț
SELECT 
    p.p_name, 
    MAX(l.l_extendedprice) - MIN(l.l_extendedprice) AS price_variation
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY price_variation DESC;

-- 159. Top 5 furnizori cu cea mai mare creștere procentuală în livrări
SELECT 
    s.s_name,
    (SUM(l.l_quantity) - LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate)) / LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) AS growth_rate
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name, s.s_suppkey, o.o_orderdate
ORDER BY growth_rate DESC
LIMIT 5;

-- 160. Produse cu variație mare în timpul de livrare
SELECT 
    p.p_name, 
    MAX(l.l_shipdate - l.l_commitdate) - MIN(l.l_shipdate - l.l_commitdate) AS delivery_variation
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY delivery_variation DESC;

-- 161. Produse care au fost cumpărate mai des în a doua jumătate a anului
SELECT 
    p.p_name,
    SUM(CASE WHEN EXTRACT(MONTH FROM o.o_orderdate) > 6 THEN 1 ELSE 0 END) AS second_half_orders,
    SUM(CASE WHEN EXTRACT(MONTH FROM o.o_orderdate) <= 6 THEN 1 ELSE 0 END) AS first_half_orders
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY p.p_name
HAVING second_half_orders > first_half_orders;

-- 162. Cele mai mari diferențe între prețurile minime și maxime plătite per produs
SELECT 
    p.p_name, 
    MAX(l.l_extendedprice) - MIN(l.l_extendedprice) AS price_diff
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY price_diff DESC;

-- 163. Clienți care comandă doar în anumite luni
SELECT 
    c.c_name, 
    EXTRACT(MONTH FROM o.o_orderdate) AS order_month,
    COUNT(o.o_orderkey) AS total_orders
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name, order_month;

-- 164. Produse care au fost reduse de cel puțin trei ori
SELECT 
    p.p_name,
    COUNT(DISTINCT l.l_discount) AS discount_count
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
WHERE l.l_discount > 0
GROUP BY p.p_name
HAVING COUNT(DISTINCT l.l_discount) >= 3;

-- 165. Clienți care au plasat o comandă mare după una mică
SELECT 
    c.c_name, 
    o.o_totalprice,
    LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS prev_order_value,
    CASE 
        WHEN o.o_totalprice > LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) * 2 THEN 'Significant Increase'
        ELSE 'Stable'
    END AS order_pattern
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey;

-- 166. Cele mai mari întârzieri de plată
SELECT 
    o.o_orderkey, 
    (l.l_receiptdate - l.l_commitdate) AS payment_delay
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
ORDER BY payment_delay DESC;

-- 167. Produse cu cea mai mare volatilitate a prețului
SELECT 
    p.p_name, 
    VARIANCE(l.l_extendedprice) AS price_volatility
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY price_volatility DESC;

-- 168. Națiuni cu cea mai mare creștere a comenzilor
WITH OrderGrowth AS (
    SELECT 
        n.n_name, 
        EXTRACT(YEAR FROM o.o_orderdate) AS order_year, 
        COUNT(o.o_orderkey) AS total_orders,
        LAG(COUNT(o.o_orderkey)) OVER (PARTITION BY n.n_name ORDER BY EXTRACT(YEAR FROM o.o_orderdate)) AS prev_year_orders
    FROM nation n
    JOIN customer c ON n.n_nationkey = c.c_nationkey
    JOIN orders o ON c.c_custkey = o.o_custkey
    GROUP BY n.n_name, order_year
)
SELECT 
    n_name, 
    order_year, 
    total_orders, 
    prev_year_orders, 
    total_orders - COALESCE(prev_year_orders, 0) AS growth
FROM OrderGrowth;

-- 169. Cele mai mari vânzări lunare
SELECT 
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    SUM(o.o_totalprice) AS total_sales
FROM orders o
GROUP BY year, month
ORDER BY total_sales DESC;

-- 170. Cele mai multe returnări de produse pe furnizor
SELECT 
    s.s_name,
    COUNT(l.l_returnflag) AS total_returns
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
WHERE l.l_returnflag = 'R'
GROUP BY s.s_name
ORDER BY total_returns DESC;

-- 171. Clienți care au avut cea mai mare variație a valorii comenzilor
SELECT 
    c.c_name,
    MAX(o.o_totalprice) - MIN(o.o_totalprice) AS order_variation
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
ORDER BY order_variation DESC;

-- 172. Identificarea comenzilor cu un model fluctuant al reducerilor
SELECT 
    o.o_orderkey,
    l.l_discount,
    LEAD(l.l_discount) OVER (PARTITION BY o.o_orderkey ORDER BY l.l_discount) AS next_discount,
    CASE 
        WHEN LEAD(l.l_discount) OVER (PARTITION BY o.o_orderkey ORDER BY l.l_discount) > l.l_discount THEN 'Increasing'
        ELSE 'Decreasing or Stable'
    END AS discount_trend
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey;

-- 173. Produse care au fost cumpărate doar în prima jumătate a anului
SELECT 
    p.p_name
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
WHERE EXTRACT(MONTH FROM o.o_orderdate) <= 6
GROUP BY p.p_name;

-- 174. Produse care au avut cele mai mari reduceri procentuale
SELECT 
    p.p_name,
    MAX(l.l_discount) AS max_discount
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY p.p_name
ORDER BY max_discount DESC;

-- 175. Clienți care au făcut comenzi de valoare variabilă
SELECT 
    c.c_name,
    STDDEV(o.o_totalprice) AS price_variability
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
ORDER BY price_variability DESC;

-- 176. Clienți care și-au schimbat preferința de furnizor
SELECT 
    c.c_name,
    COUNT(DISTINCT s.s_name) AS supplier_count
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN partsupp ps ON l.l_partkey = ps.ps_partkey
JOIN supplier s ON ps.ps_suppkey = s.s_suppkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT s.s_name) > 1;

-- 177. Clienți care au comandat în mod constant în fiecare lună a anului
SELECT 
    c.c_name
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT EXTRACT(MONTH FROM o.o_orderdate)) = 12;

-- 178. Furnizori care au crescut constant volumul de livrări
SELECT 
    s.s_name,
    SUM(l.l_quantity) AS current_volume,
    LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) AS prev_volume,
    CASE 
        WHEN SUM(l.l_quantity) > LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) THEN 'Increasing'
        ELSE 'Decreasing or Stable'
    END AS supply_trend
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name, s.s_suppkey, o.o_orderdate;

-- 179. Furnizori care au pierdut cel mai mult din volum
SELECT 
    s.s_name,
    SUM(l.l_quantity) AS current_volume,
    LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate) AS prev_volume,
    SUM(l.l_quantity) - COALESCE(LAG(SUM(l.l_quantity)) OVER (PARTITION BY s.s_suppkey ORDER BY o.o_orderdate), 0) AS loss
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name, s.s_suppkey, o.o_orderdate;

-- 180. Clienți care au cumpărat de la furnizori din cel puțin 5 țări diferite
SELECT 
    c.c_name,
    COUNT(DISTINCT n.n_name) AS num_countries
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN supplier s ON l.l_suppkey = s.s_suppkey
JOIN nation n ON s.s_nationkey = n.n_nationkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT n.n_name) >= 5;

-- 181. Produse care au avut cele mai mari diferențe de preț între furnizori
SELECT 
    p.p_name,
    MAX(ps.ps_supplycost) - MIN(ps.ps_supplycost) AS price_variability
FROM part p
JOIN partsupp ps ON p.p_partkey = ps.ps_partkey
GROUP BY p.p_name
ORDER BY price_variability DESC;

-- 182. Clienți care au cumpărat cel puțin 3 produse diferite per comandă
SELECT 
    c.c_name,
    COUNT(DISTINCT l.l_partkey) AS unique_products
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
GROUP BY c.c_name
HAVING COUNT(DISTINCT l.l_partkey) >= 3;

-- 183. Cele mai scumpe produse achiziționate per client
SELECT 
    c.c_name,
    p.p_name,
    MAX(l.l_extendedprice) AS highest_price
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN part p ON l.l_partkey = p.p_partkey
GROUP BY c.c_name, p.p_name
ORDER BY highest_price DESC;

-- 184. Clienți care au plasat doar o comandă mare
SELECT 
    c.c_name
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
WHERE o.o_totalprice > (SELECT AVG(o2.o_totalprice) * 2 FROM orders o2)
GROUP BY c.c_name;

-- 185. Clienți care au cumpărat cel mai frecvent de la același furnizor
SELECT 
    c.c_name,
    s.s_name,
    COUNT(o.o_orderkey) AS total_orders
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
JOIN partsupp ps ON l.l_partkey = ps.ps_partkey
JOIN supplier s ON ps.ps_suppkey = s.s_suppkey
GROUP BY c.c_name, s.s_name
ORDER BY total_orders DESC;

-- 186. Cele mai cumpărate produse per furnizor
SELECT 
    s.s_name,
    p.p_name,
    COUNT(l.l_partkey) AS total_purchases
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN part p ON ps.ps_partkey = p.p_partkey
JOIN lineitem l ON p.p_partkey = l.l_partkey
GROUP BY s.s_name, p.p_name
ORDER BY total_purchases DESC;

-- 187. Cele mai rapide metode de livrare per produs 
SELECT 
    p.p_name,
    l.l_shipmode,
    MIN(l.l_shipdate - o.o_orderdate) AS fastest_delivery
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY p.p_name, l.l_shipmode
ORDER BY fastest_delivery ASC;

-- 188. Clienți care au avut cele mai mari diferențe între comenzile consecutive
SELECT 
    c.c_name,
    o.o_orderdate,
    o.o_totalprice,
    LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS prev_order_price,
    o.o_totalprice - LAG(o.o_totalprice) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS price_difference
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey;

-- 189. Cele mai mari comenzi per furnizor într-un an
SELECT 
    s.s_name,
    EXTRACT(YEAR FROM o.o_orderdate) AS order_year,
    MAX(o.o_totalprice) AS max_order
FROM supplier s
JOIN partsupp ps ON s.s_suppkey = ps.ps_suppkey
JOIN lineitem l ON ps.ps_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY s.s_name, order_year;

-- 190. Clienți care au avut discounturi din ce în ce mai mari
SELECT 
    c.c_name,
    l.l_discount,
    LEAD(l.l_discount) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) AS next_discount,
    CASE 
        WHEN LEAD(l.l_discount) OVER (PARTITION BY c.c_custkey ORDER BY o.o_orderdate) > l.l_discount THEN 'Increasing'
        ELSE 'Stable or Decreasing'
    END AS trend
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
JOIN lineitem l ON o.o_orderkey = l.l_orderkey;

-- 191. Cele mai frecvente zile pentru plasarea comenzilor
SELECT 
    EXTRACT(DOW FROM o.o_orderdate) AS day_of_week,
    COUNT(o.o_orderkey) AS order_count
FROM orders o
GROUP BY day_of_week
ORDER BY order_count DESC;

-- 192. Produse care au fost returnate cel mai repede
SELECT 
    p.p_name,
    MIN(l.l_returnflag) AS earliest_return
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
WHERE l.l_returnflag = 'R'
GROUP BY p.p_name
ORDER BY earliest_return ASC;

-- 193. Cele mai frecvente motive de întârziere a livrărilor
SELECT 
    l.l_shipmode,
    AVG(l.l_shipdate - l.l_commitdate) AS avg_delay
FROM lineitem l
GROUP BY l.l_shipmode
ORDER BY avg_delay DESC;

-- 194. Produse care au avut cele mai mari vânzări per lună
SELECT 
    p.p_name,
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    EXTRACT(MONTH FROM o.o_orderdate) AS month,
    SUM(l.l_extendedprice) AS total_sales
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
GROUP BY p.p_name, year, month
ORDER BY total_sales DESC;

-- 195. Clienți care au plasat doar comenzi sub media tuturor comenzilor
SELECT 
    c.c_name
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name
HAVING MAX(o.o_totalprice) < (SELECT AVG(o_totalprice) FROM orders);

-- 196. Cele mai comune zile ale săptămânii pentru returnarea produselor
SELECT 
    EXTRACT(DOW FROM o.o_orderdate) AS return_day,
    COUNT(l.l_returnflag) AS return_count
FROM orders o
JOIN lineitem l ON o.o_orderkey = l.l_orderkey
WHERE l.l_returnflag = 'R'
GROUP BY return_day
ORDER BY return_count DESC;

-- 197. Produse care au fost cumpărate doar în zile de weekend
SELECT 
    p.p_name
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
WHERE EXTRACT(DOW FROM o.o_orderdate) IN (6, 7)
GROUP BY p.p_name;

-- 198. Clienți care au făcut cea mai mare creștere a valorii comenzilor într-un an
SELECT 
    c.c_name,
    EXTRACT(YEAR FROM o.o_orderdate) AS year,
    SUM(o.o_totalprice) AS total_spent,
    LAG(SUM(o.o_totalprice)) OVER (PARTITION BY c.c_custkey ORDER BY EXTRACT(YEAR FROM o.o_orderdate)) AS prev_year_spent,
    SUM(o.o_totalprice) - COALESCE(LAG(SUM(o.o_totalprice)) OVER (PARTITION BY c.c_custkey ORDER BY EXTRACT(YEAR FROM o.o_orderdate)), 0) AS growth
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
GROUP BY c.c_name, c.c_custkey, year
ORDER BY growth DESC;

-- 199. Produse care au fost cumpărate doar în zilele de luni
SELECT 
    p.p_name
FROM part p
JOIN lineitem l ON p.p_partkey = l.l_partkey
JOIN orders o ON l.l_orderkey = o.o_orderkey
WHERE EXTRACT(DOW FROM o.o_orderdate) = 1
GROUP BY p.p_name;

-- 200. Clienți care au făcut comenzi în zilele cu cel mai mare volum de comenzi
SELECT 
    c.c_name,
    o.o_orderdate,
    COUNT(*) OVER (PARTITION BY o.o_orderdate) AS daily_orders
FROM customer c
JOIN orders o ON c.c_custkey = o.o_custkey
ORDER BY daily_orders DESC;



