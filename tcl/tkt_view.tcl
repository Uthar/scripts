<table class='ticket-metadata' cellpadding="5">

<tr>
<td class="tktDspLabel">Title:</td>
<td class="tktDspValue"><strong>$<title></strong></td>
</tr>

<tr><td class="tktDspLabel">Ticket&nbsp;Hash:</td>
<th1>
if {[info exists tkt_uuid]} {
  html "<td class='tktDspValue' colspan='3'>"
  copybtn hash-tk 0 $tkt_uuid 2
  if {[hascap s]} {
    html " ($tkt_id)"
  }
  html "</td></tr>\n"
} else {
  if {[hascap s]} {
    html "<td class='tktDspValue' colspan='3'>Deleted "
    html "(0)</td></tr>\n"
  } else {
    html "<td class='tktDspValue' colspan='3'>Deleted</td></tr>\n"
  }
}
</th1>

<tr>
<td class="tktDspLabel">Status:</td>
<th1>
html "<td class='tktDspValue'>"
html "<div class='$status'>$status</div>"
html "</td>"
</th1>
</tr>

<tr>
<td class="tktDspLabel">Type:</td>
<td class="tktDspValue">$<type></td>
</tr>

<tr>
<td class="tktDspLabel">Severity:</td>
<td class="tktDspValue">$<severity></td>
</tr>

<tr>
<td class="tktDspLabel">Last&nbsp;Modified:</td>
<td class="tktDspValue">
<th1>
if {[info exists tkt_datetime]} {
  html $tkt_datetime
}
</th1>
</td>
</tr>

<th1>enable_output [hascap e]</th1>
  <th1>
  if {[string length $private_contact]>0} {
    html "<tr>"
    html "<td class='tktDspLabel'>Contact:</td><td class='tktDspValue'>"
    $private_contact
    html "</td>"
    html "</tr>"
  }
  </th1>
<th1>enable_output 1</th1>

<tr>
<td class="tktDspLabel">Version&nbsp;Found&nbsp;In:</td>
<td colspan="3" valign="top" class="tktDspValue"><a href="/info/$<foundin>">$<foundin></a></td>
</tr>

<th1>
if {[info exists comment]} {
  if {[string length $comment]>10} {
    html {
      <tr><td class="tktDspLabel">Description:</td></tr>
      <tr><td colspan="5" class="tktDspValue">
    }
    if {[info exists plaintext]} {
      set r [randhex]
      wiki "<verbatim-$r links>\n$comment\n</verbatim-$r>"
    } else {
      wiki $comment
    }
  }
}
</th1>
</table>

<th1>
set seenRow 0
set alwaysPlaintext [info exists plaintext]
query {SELECT datetime(tkt_mtime) AS xdate, login AS xlogin,
              mimetype as xmimetype, icomment AS xcomment,
              username AS xusername
         FROM ticketchng
        WHERE tkt_id=$tkt_id AND length(icomment)>0} {
  html "<div class='ticket-comment'>"
  html "<div class='ticket-comment-user'>"
  html "[htmlize $xlogin]"
  if {$xlogin ne $xusername && [string length $xusername]>0} {
    html " (claiming to be [htmlize $xusername])"
  }
  html " commented on $xdate"
  html "</div>\n"
  html "<div class='ticket-comment-content'>"
  if {$alwaysPlaintext || $xmimetype eq "text/plain"} {
    set r [randhex]
    if {$xmimetype ne "text/plain"} {html "([htmlize $xmimetype])\n"}
    wiki "<verbatim-$r>[string trimright $xcomment]</verbatim-$r>\n"
  } elseif {$xmimetype eq "text/x-fossil-wiki"} {
    wiki "<p>\n[string trimright $xcomment]\n</p>\n"
  } elseif {$xmimetype eq "text/x-markdown"} {
    html [lindex [markdown $xcomment] 1]
  } elseif {$xmimetype eq "text/html"} {
    wiki "<p><nowiki>\n[string trimright $xcomment]\n</nowiki>\n"
  } else {
    set r [randhex]
    wiki "<verbatim-$r links>[string trimright $xcomment]</verbatim-$r>\n"
  }
html "</div>"
html "</div>"
}
</th1>
