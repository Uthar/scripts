
 -- Find all the primary parents of the tip check-in.
 -- Like git log --first-parent
 with recursive first_parent(parent, child) as (
  select pid, cid from plink
  where cid = (select objid from event order by mtime desc limit 1)
  and isprim = 1
  union
  select pid, cid from plink, first_parent
  where cid = parent and isprim = 1
) select parent, child, comment, type
  from first_parent fp, blob b, event e
  where b.rid = fp.child
  and e.objid = b.rid
  order by e.mtime desc
