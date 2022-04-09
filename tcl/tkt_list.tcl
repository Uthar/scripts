
<th1> # -*- mode: tcl -*-

set pagesize [getParameter pagesize 30]

if {[expr "$pagesize <= 0"]} {
  set pagesize 1
}

set search [getParameter search ""]

query "select count(*) as cnt from (select * from ticket where title like '%$search%')" {
  set all $cnt
}

if {[expr "$pagesize > $all"]} {
  set pagesize $all
}

set pages [expr "$all / $pagesize"]

for {} {[expr "$pages * $pagesize < $all"]} {} {
  set pages [expr "$pages + 1"]
}

set page [getParameter page 1]
if {[expr "$page <= 0"]} {
  set page 1
}
if {[expr "$page > $pages"]} {
  set page $pages
}

html "<form method='GET' action='$home/ticket'>"
html "<label for='pagesize'>Page Size</label>"
html "<input name='pagesize' type='number' value='$pagesize'>\n"
html "<label for='page'>Page</label>"
html "<input name='page' type='number' value='$page'>\n"
html "<label for='search'>Title Search</label>"
html "<input name='search' type='text' value='$search'>\n"
html "<input type='submit'>"
html "</form>"

set offset [expr "$page * $pagesize - $pagesize"]
set limit $pagesize

html "<h4>Tickets</h4>"
html "<table>"
html "<thead><tr><td>Priority</td><td>Status</td><td>Title</td></tr></thead>"
html "<tbody>"
html "<th1>"

query "select * from ticket where title like '%$search%' limit $limit offset $offset" {
  html "<tr>"
  html "<td>$severity</td>"
  html "<td>$status</td>"
  html "<td><a href='$home/tktview?name=$tkt_uuid'>$title</a></td>"
  html "</tr>"
}

html "</tbody></table>\n"

html "<h6>page $page of $pages</h6>"


</th1>
