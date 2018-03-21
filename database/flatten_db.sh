#!/bin/bash
sqlite3 exercise01.sqlite <<EOF
.headers on
.mode csv
.output flattened.csv
SELECT t1.id, t1.age, t1.capital_gain, t1.capital_loss, t1.hours_week, t1.over_50k, 
t2.name as workclass,
t3.name as education,
t4.name as marital_status,
t5.name as occupation,
t6.name as relationship,
t7.name as race,
t8.name as sex,
t9.name as country
FROM records t1 
LEFT JOIN workclasses as t2 ON t2.id = t1.workclass_id
LEFT JOIN education_levels as t3 ON t3.id = t1.education_num
LEFT JOIN marital_statuses as t4 ON t4.id = t1.marital_status_id
LEFT JOIN occupations as t5 ON t5.id = t1.occupation_id
LEFT JOIN relationships as t6 ON t6.id = t1.relationship_id
LEFT JOIN races as t7 ON t7.id = t1.race_id
LEFT JOIN sexes as t8 ON t8.id = t1.sex_id
LEFT JOIN countries as t9 ON t9.id = t1.country_id;
EOF