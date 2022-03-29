-- Kind of like file viewer on gitlab/github

-- Show latest changes to:

-- one file
select f.name as name, e.comment, datetime(e.mtime) as d
from filename f, event e, mlink m
where f.name='flake.nix'
and f.fnid=m.fnid
and e.objid=m.mid
and e.type='ci'
order by d desc
limit 1

-- dir
select f.name as name, e.comment, datetime(e.mtime) as d
from filename f, event e, mlink m
where f.name like 'modules/%'
and f.fnid=m.fnid
and e.objid=m.mid
and e.type='ci'
order by d desc
limit 1

-- all files

select f.name as name, e.comment, datetime(e.mtime) as d
from filename f, event e, mlink m
where f.fnid=m.fnid
and e.objid=m.mid
and e.type='ci'
group by name,d
order by d desc
