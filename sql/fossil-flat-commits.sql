-- select last 20 commits, flat, like git log does by default
select datetime(e.mtime) as datetime, b.uuid, e.comment, tx.value as branch
from event e
join blob b on b.rid=e.objid
join tagxref tx on b.rid=tx.rid
join tag t on tx.tagid=t.tagid
where e.type='ci' and t.tagname='branch'
order by datetime desc
limit 20
