CREATE TABLE loc_nums (id_maison TEXT, n INTEGER);
INSERT INTO loc_nums SELECT idm, n FROM (
    SELECT COUNT(*) AS n, M.id_maison as idm
    FROM maison AS M, locataires AS L, compte AS C
        WHERE L.ci = C.ci AND C.id_maison = M.id_maison GROUP BY M.id_maison
    UNION
    SELECT 0 AS n, M.id_maison as idm FROM maison AS M
        WHERE M.id_maison NOT IN (
            SELECT M.id_maison FROM maison AS M JOIN compte AS C ON M.id_maison = C.id_maison)
);

SELECT AVG(n), AVG(n*n) - AVG(n)*AVG(n) FROM loc_nums,

