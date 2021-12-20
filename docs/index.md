# diagramma URI

      <style type="text/css">
    ::-moz-selection
    {
      color: #FFFAF0;
      background: #0F0A00;
    }
    ::selection
    {
      color: #FFFAF0;
      background: #0F0A00;
    }
    .ebnf a, .grammar a
    {
      text-decoration: none;
    }
    .ebnf a:hover, .grammar a:hover
    {
      color: #050300;
      text-decoration: underline;
    }
    .signature
    {
      color: #805500;
      font-size: 11px;
      text-align: right;
    }
    body
    {
      font: normal 12px Verdana, sans-serif;
      color: #0F0A00;
      background: #FFFAF0;
    }
    a:link, a:visited
    {
      color: #0F0A00;
    }
    a:link.signature, a:visited.signature
    {
      color: #805500;
    }
    a.button, #tabs li a
    {
      padding: 0.25em 0.5em;
      border: 1px solid #805500;
      background: #F1E2C6;
      color: #805500;
      text-decoration: none;
      font-weight: bold;
    }
    a.button:hover, #tabs li a:hover
    {
      color: #050300;
      background: #FFF0D1;
      border-color: #050300;
    }
    #tabs
    {
      padding: 3px 10px;
      margin-left: 0;
      margin-top: 58px;
      border-bottom: 1px solid #0F0A00;
    }
    #tabs li
    {
      list-style: none;
      margin-left: 5px;
      display: inline;
    }
    #tabs li a
    {
      border-bottom: 1px solid #0F0A00;
    }
    #tabs li a.active
    {
      color: #0F0A00;
      background: #FFFAF0;
      border-color: #0F0A00;
      border-bottom: 1px solid #FFFAF0;
      outline: none;
    }
    #divs div
    {
      display: none;
      overflow:auto;
    }
    #divs div.active
    {
      display: block;
    }
    #text
    {
      border-color: #805500;
      background: #FFFDFA;
      color: #050300;
    }
    .small
    {
      vertical-align: top;
      text-align: right;
      font-size: 9px;
      font-weight: normal;
      line-height: 120%;
    }
    td.small
    {
      padding-top: 0px;
    }
    .hidden
    {
      visibility: hidden;
    }
    td:hover .hidden
    {
      visibility: visible;
    }
    div.download
    {
      display: none;
      background: #FFFAF0;
      position: absolute;
      right: 34px;
      top: 94px;
      padding: 10px;
      border: 1px dotted #0F0A00;
    }
    #divs div.ebnf, .ebnf code
    {
      display: block;
      padding: 10px;
      background: #FFF0D1;
      width: 992px;
    }
    #divs div.grammar
    {
      display: block;
      padding-left: 16px;
      padding-top: 2px;
      padding-bottom: 2px;
      background: #FFF0D1;
    }
    pre
    {
      margin: 0px;
    }
    .ebnf div
    {
      padding-left: 13ch;
      text-indent: -13ch;
    }
    .ebnf code, .grammar code, textarea, pre
    {
      font:12px SFMono-Regular,Consolas,Liberation Mono,Menlo,Courier,monospace;
    }
    tr.option-line td:first-child
    {
      text-align: right
    }
    tr.option-text td
    {
      padding-bottom: 10px
    }
    table.palette
    {
      border-top: 1px solid #050300;
      border-right: 1px solid #050300;
      margin-bottom: 4px
    }
    td.palette
    {
      border-bottom: 1px solid #050300;
      border-left: 1px solid #050300;
    }
    a.palette
    {
      padding: 2px 3px 2px 10px;
      text-decoration: none;
    }
    .palette
    {
      -webkit-user-select: none;
      -khtml-user-select: none;
      -moz-user-select: none;
      -o-user-select: none;
      -ms-user-select: none;
    }
  </style>

      <p style="font-size: 14px; font-weight:bold"><a name="URI">URI:</a></p><img border="0" src="diagram/URI.png" height="289" width="457" usemap="#URI.map"><map name="URI.map"><area shape="rect" coords="49,1,119,33" href="#Scheme" title="Scheme"><area shape="rect" coords="183,1,279,33" href="#Generic-URI" title="Generic-URI"><area shape="rect" coords="175,45,247,77" href="#Userinfo" title="Userinfo"><area shape="rect" coords="339,77,387,109" href="#Host" title="Host"><area shape="rect" coords="169,121,217,153" href="#Host" title="Host"><area shape="rect" coords="193,165,265,197" href="#Userinfo" title="Userinfo"><area shape="rect" coords="155,253,225,285" href="#Zos-URI" title="Zos-URI"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#URI" title="URI">URI</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Scheme" title="Scheme">Scheme</a> ':' <a href="#Generic-URI" title="Generic-URI">Generic-URI</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| 'mailto' ':' <a href="#Userinfo" title="Userinfo">Userinfo</a> ( '@' <a href="#Host" title="Host">Host</a> )?</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| 'news' ':' <a href="#Host" title="Host">Host</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| ( 'tel' | 'fax' ) ':' <a href="#Userinfo" title="Userinfo">Userinfo</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| 'zos' ':' <a href="#Zos-URI" title="Zos-URI">Zos-URI</a></div></code></div>
         </p>
      
      <p>no references</p><br><p style="font-size: 14px; font-weight:bold"><a name="Generic-URI">Generic-URI:</a></p><img border="0" src="diagram/Generic-URI.png" height="167" width="727" usemap="#Generic-URI.map"><map name="Generic-URI.map"><area shape="rect" coords="49,1,127,33" href="#Authority" title="Authority"><area shape="rect" coords="235,65,283,97" href="#Path" title="Path"><area shape="rect" coords="389,65,447,97" href="#Query" title="Query"><area shape="rect" coords="557,65,637,97" href="#Fragment" title="Fragment"><area shape="rect" coords="157,131,205,163" href="#Path" title="Path"><area shape="rect" coords="311,131,369,163" href="#Query" title="Query"><area shape="rect" coords="479,131,559,163" href="#Fragment" title="Fragment"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Generic-URI" title="Generic-URI">Generic-URI</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Authority" title="Authority">Authority</a> ( '/' <a href="#Path" title="Path">Path</a>? ( '?' <a href="#Query" title="Query">Query</a> )? ( '#' <a href="#Fragment" title="Fragment">Fragment</a> )? )?</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '/'? <a href="#Path" title="Path">Path</a>? ( '?' <a href="#Query" title="Query">Query</a> )? ( '#' <a href="#Fragment" title="Fragment">Fragment</a> )?</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#URI" title="URI">URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Zos-URI">Zos-URI:</a></p><img border="0" src="diagram/Zos-URI.png" height="167" width="757" usemap="#Zos-URI.map"><map name="Zos-URI.map"><area shape="rect" coords="49,1,127,33" href="#Authority" title="Authority"><area shape="rect" coords="235,65,313,97" href="#Zos-path" title="Zos-path"><area shape="rect" coords="419,65,477,97" href="#Query" title="Query"><area shape="rect" coords="587,65,667,97" href="#Fragment" title="Fragment"><area shape="rect" coords="157,131,235,163" href="#Zos-path" title="Zos-path"><area shape="rect" coords="341,131,399,163" href="#Query" title="Query"><area shape="rect" coords="509,131,589,163" href="#Fragment" title="Fragment"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Zos-URI" title="Zos-URI">Zos-URI</a>&nbsp;&nbsp;::= <a href="#Authority" title="Authority">Authority</a> ( '/' <a href="#Zos-path" title="Zos-path">Zos-path</a>? ( '?' <a href="#Query" title="Query">Query</a> )? ( '#' <a href="#Fragment" title="Fragment">Fragment</a> )? )?</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '/'? <a href="#Zos-path" title="Zos-path">Zos-path</a>? ( '?' <a href="#Query" title="Query">Query</a> )? ( '#' <a href="#Fragment" title="Fragment">Fragment</a> )?</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#URI" title="URI">URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Scheme">Scheme:</a></p><img border="0" src="diagram/Scheme.png" height="37" width="163" usemap="#Scheme.map"><map name="Scheme.map"><area shape="rect" coords="29,1,133,33" href="#Identificatore" title="Identificatore"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Scheme" title="Scheme">Scheme</a>&nbsp;&nbsp;&nbsp;::= <a href="#Identificatore" title="Identificatore">Identificatore</a></div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#URI" title="URI">URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Authority">Authority:</a></p><img border="0" src="diagram/Authority.png" height="69" width="497" usemap="#Authority.map"><map name="Authority.map"><area shape="rect" coords="105,33,177,65" href="#Userinfo" title="Userinfo"><area shape="rect" coords="269,1,317,33" href="#Host" title="Host"><area shape="rect" coords="401,33,447,65" href="#Port" title="Port"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Authority" title="Authority">Authority</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= '//' ( <a href="#Userinfo" title="Userinfo">Userinfo</a> '@' )? <a href="#Host" title="Host">Host</a> ( ':' <a href="#Port" title="Port">Port</a> )?</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Generic-URI" title="Generic-URI">Generic-URI</a></li>
            
            <li><a href="#Zos-URI" title="Zos-URI">Zos-URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Userinfo">Userinfo:</a></p><img border="0" src="diagram/Userinfo.png" height="37" width="163" usemap="#Userinfo.map"><map name="Userinfo.map"><area shape="rect" coords="29,1,133,33" href="#Identificatore" title="Identificatore"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Userinfo" title="Userinfo">Userinfo</a>&nbsp;::= <a href="#Identificatore" title="Identificatore">Identificatore</a></div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Authority" title="Authority">Authority</a></li>
            
            <li><a href="#URI" title="URI">URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Host">Host:</a></p><img border="0" src="diagram/Host.png" height="125" width="277" usemap="#Host.map"><map name="Host.map"><area shape="rect" coords="69,45,207,77" href="#Identificatore-host" title="Identificatore-host"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Host" title="Host">Host</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Identificatore-host" title="Identificatore-host">Identificatore-host</a> ( '.' <a href="#Identificatore-host" title="Identificatore-host">Identificatore-host</a> )*</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| 'NNN.NNN.NNN.NNN'</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Authority" title="Authority">Authority</a></li>
            
            <li><a href="#URI" title="URI">URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Port">Port:</a></p><img border="0" src="diagram/Port.png" height="53" width="147" usemap="#Port.map"><map name="Port.map"><area shape="rect" coords="49,17,97,49" href="#Digit" title="Digit"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Port" title="Port">Port</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Digit" title="Digit">Digit</a>+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Authority" title="Authority">Authority</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Path">Path:</a></p><img border="0" src="diagram/Path.png" height="81" width="203" usemap="#Path.map"><map name="Path.map"><area shape="rect" coords="49,45,153,77" href="#Identificatore" title="Identificatore"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Path" title="Path">Path</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Identificatore" title="Identificatore">Identificatore</a> ( '/' <a href="#Identificatore" title="Identificatore">Identificatore</a> )*</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Generic-URI" title="Generic-URI">Generic-URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Zos-path">Zos-path:</a></p><img border="0" src="diagram/Zos-path.png" height="69" width="299" usemap="#Zos-path.map"><map name="Zos-path.map"><area shape="rect" coords="29,1,77,33" href="#Id44" title="Id44"><area shape="rect" coords="163,33,203,65" href="#Id8" title="Id8"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Zos-path" title="Zos-path">Zos-path</a>&nbsp;::= <a href="#Id44" title="Id44">Id44</a> ( '(' <a href="#Id8" title="Id8">Id8</a> ')' )?</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Zos-URI" title="Zos-URI">Zos-URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Id44">Id44:</a></p><img border="0" src="diagram/Id44.png" height="97" width="195" usemap="#Id44.map"><map name="Id44.map"><area shape="rect" coords="69,17,125,49" href="#Alpha" title="Alpha"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Id44" title="Id44">Id44</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= ( <a href="#Alpha" title="Alpha">Alpha</a> | '.' )+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Zos-path" title="Zos-path">Zos-path</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Id8">Id8:</a></p><img border="0" src="diagram/Id8.png" height="53" width="155" usemap="#Id8.map"><map name="Id8.map"><area shape="rect" coords="49,17,105,49" href="#Alpha" title="Alpha"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Id8" title="Id8">Id8</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Alpha" title="Alpha">Alpha</a>+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Zos-path" title="Zos-path">Zos-path</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Query">Query:</a></p><img border="0" src="diagram/Query.png" height="37" width="205" usemap="#Query.map"><map name="Query.map"><area shape="rect" coords="29,1,175,33" href="#Identificatore-query" title="Identificatore-query"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Query" title="Query">Query</a>&nbsp;&nbsp;&nbsp;&nbsp;::= <a href="#Identificatore-query" title="Identificatore-query">Identificatore-query</a></div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Generic-URI" title="Generic-URI">Generic-URI</a></li>
            
            <li><a href="#Zos-URI" title="Zos-URI">Zos-URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Fragment">Fragment:</a></p><img border="0" src="diagram/Fragment.png" height="37" width="227" usemap="#Fragment.map"><map name="Fragment.map"><area shape="rect" coords="29,1,197,33" href="#Identificatore-fragment" title="Identificatore-fragment"></map><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Fragment" title="Fragment">Fragment</a>&nbsp;::= <a href="#Identificatore-fragment" title="Identificatore-fragment">Identificatore-fragment</a></div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Generic-URI" title="Generic-URI">Generic-URI</a></li>
            
            <li><a href="#Zos-URI" title="Zos-URI">Zos-URI</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Identificatore-fragment">Identificatore-fragment:</a></p><img border="0" src="diagram/Identificatore-fragment.png" height="53" width="177"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Identificatore-fragment" title="Identificatore-fragment">Identificatore-fragment</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= 'caratteri'+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Fragment" title="Fragment">Fragment</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Identificatore-query">Identificatore-query:</a></p><img border="0" src="diagram/Identificatore-query.png" height="53" width="233"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Identificatore-query" title="Identificatore-query">Identificatore-query</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= 'caratteri senza #'+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Query" title="Query">Query</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Identificatore">Identificatore:</a></p><img border="0" src="diagram/Identificatore.png" height="53" width="283"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Identificatore" title="Identificatore">Identificatore</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= 'caratteri senza # ? / @ :'+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Path" title="Path">Path</a></li>
            
            <li><a href="#Scheme" title="Scheme">Scheme</a></li>
            
            <li><a href="#Userinfo" title="Userinfo">Userinfo</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Identificatore-host">Identificatore-host:</a></p><img border="0" src="diagram/Identificatore-host.png" height="53" width="291"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Identificatore-host" title="Identificatore-host">Identificatore-host</a></div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;::= 'caratteri senza # ? / @ : .'+</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Host" title="Host">Host</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Digit">Digit:</a></p><img border="0" src="diagram/Digit.png" height="433" width="127"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Digit" title="Digit">Digit</a>&nbsp;&nbsp;&nbsp;&nbsp;::= '0'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '1'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '2'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '3'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '4'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '5'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '6'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '7'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '8'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '9'</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Port" title="Port">Port</a></li>
            </ul>
         </p><br><p style="font-size: 14px; font-weight:bold"><a name="Alpha">Alpha:</a></p><img border="0" src="diagram/Alpha.png" height="477" width="163"><p>
         
         <div class="ebnf"><code>
               
               <div><a href="#Alpha" title="Alpha">Alpha</a>&nbsp;&nbsp;&nbsp;&nbsp;::= '0'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '1'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '2'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '3'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '4'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '5'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '6'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '7'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '8'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| '9'</div>
               
               <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| 'a-zA-Z'</div></code></div>
         </p>
      
      <p>referenced by:
         
         <ul>
            
            <li><a href="#Id44" title="Id44">Id44</a></li>
            
            <li><a href="#Id8" title="Id8">Id8</a></li>
            </ul>
         </p><br><hr>
      
      <p>
         
         <table class="signature" border="0">
            
            <tr>
               
               <td style="width: 100%">&nbsp;</td>
               
               <td valign="top">
                  
                  <nobr class="signature">... generated by <a name="Railroad-Diagram-Generator" class="signature" title="https://bottlecaps.de/rr/ui" href="https://bottlecaps.de/rr/ui" target="_blank">RR - Railroad Diagram Generator</a></nobr>
                  </td>
               
               <td><a name="Railroad-Diagram-Generator" title="https://bottlecaps.de/rr/ui" href="https://bottlecaps.de/rr/ui" target="_blank"><img border="0" src="diagram/rr-1.63.png" height="16" width="16"></a></td>
               </tr>
            </table>
         </p>
