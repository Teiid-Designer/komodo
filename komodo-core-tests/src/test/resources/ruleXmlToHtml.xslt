<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
  <body>
  <h2>Validation Rules</h2>
  <table border="1">
    <!-- Header -->
    <tr bgcolor="#e2e2e2">
      <th style="text-align:left">Rule ID</th>
      <th style="text-align:left">Rule Type</th>
      <th style="text-align:left">Node [addnl Prop Restriction]</th>
      <th style="text-align:left">Rule Target</th>
      <th style="text-align:left">Description</th>
      <th style="text-align:left">Severity</th>
      <th style="text-align:left">Detail</th>
    </tr>
  <xsl:for-each select="validationRuleSet/nodeValidation">
      <!-- Node Name Validation -->
      <xsl:for-each select="./nameValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>NodeNameValidation</td>
          <xsl:choose>
            <xsl:when test="../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../propRestriction" />&#160;=&#160;<xsl:value-of select="../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="../@jcrName" />&#160;[Node]</td>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">Pattern</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="pattern"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
    <!--                 -->
    <!--  Property Rules -->
    <!--                 -->
      <!-- Property valueValidation -->
      <xsl:for-each select="./propertyValidation/valueValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>PropertyValueValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">Pattern</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="pattern"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
      <!-- Property valueRangeValidation -->
      <xsl:for-each select="./propertyValidation/valueRangeValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>PropertyValueRangeValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">MinValue</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="minValue"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MaxValue</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="maxValue"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MinInclusive</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="./minValue/@inclusive"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MaxInclusive</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="./maxValue/@inclusive"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
      <!-- Property relationshipValidation -->
      <xsl:for-each select="./propertyValidation/relationshipValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>PropertyRelationshipValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Property]</td></tr>
                      <tr><td>Prop Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">PropExists</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="propExists"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">PropAbsent</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="propAbsent"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">ChildExists</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="childExists"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">ChildAbsent</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="childAbsent"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
    <!--                 -->
    <!--  Child Rules -->
    <!--                 -->
      <!-- Child sameNameSiblingValidation -->
      <xsl:for-each select="./childValidation/sameNameSiblingValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>ChildSameNameSiblingValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td>NA</td>
        </tr>
      </xsl:for-each>
    <!--                 -->
    <!--  Child Count Rule -->
    <!--                 -->
      <xsl:for-each select="./childValidation/childCountValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>ChildCountValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">MinValue</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="minValue"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MaxValue</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="maxValue"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MinInclusive</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="./minValue/@inclusive"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">MaxInclusive</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="./maxValue/@inclusive"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
      <!-- Child relationshipValidation -->
      <xsl:for-each select="./childValidation/relationshipValidation">
        <tr>
          <td><xsl:value-of select="./@id" /></td>
          <td>ChildRelationshipValidation</td>
          <xsl:choose>
            <xsl:when test="../../propRestriction">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../../@jcrName" /></td></tr>
                      <tr><td>[<xsl:value-of select="../../propRestriction" />&#160;=&#160;<xsl:value-of select="../../propRestriction/@value" />]</td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><xsl:value-of select="../../@jcrName" /></td>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="../@required">
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=<xsl:value-of select="../@required" /></td></tr>
                  </table>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td><table style="border:0px;margin:5px;padding:0px">
                      <tr><td><xsl:value-of select="../@jcrName" />&#160;[Child]</td></tr>
                      <tr><td>Child Required=false</td></tr>
                  </table>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td><xsl:value-of select="description"/></td>
          <td><xsl:value-of select="./@severity" /></td>
          <td><table style="border:1px solid black;border-collapse:collapse;margin:5px;padding:0px">
                <tr>
                  <td style="border:1px solid black;padding:5px">PropExists</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="propExists"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">PropAbsent</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="propAbsent"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">ChildExists</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="childExists"/></td>
                 </tr>
                <tr>
                  <td style="border:1px solid black;padding:5px">ChildAbsent</td>
                  <td style="border:1px solid black;padding:5px"><xsl:value-of select="childAbsent"/></td>
                 </tr>
          </table></td>
        </tr>
      </xsl:for-each>
  </xsl:for-each>
  </table>
  </body>
  </html>
</xsl:template>
</xsl:stylesheet>
