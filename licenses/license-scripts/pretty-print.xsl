<?xml version="1.0"?>

 <xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>
  <xsl:param name="indent-increment" select="'   '" />

  <xsl:template match="*">
     <xsl:param name="indent" select="'&#xA;'"/>

     <xsl:value-of select="$indent"/>
     <xsl:copy>
       <xsl:copy-of select="@*" />
       <xsl:apply-templates>
         <xsl:with-param name="indent" select="concat($indent, $indent-increment)"/>
       </xsl:apply-templates>
       <xsl:if test="count(*)>0"><xsl:value-of select="$indent"/></xsl:if>
     </xsl:copy>
  </xsl:template>

  <xsl:template match="comment()|processing-instruction()">
     <xsl:text>
</xsl:text>
     <xsl:copy />
  </xsl:template>

  <!-- WARNING: this is dangerous. Handle with care -->
  <xsl:template match="text()">
    <xsl:value-of select="normalize-space()"/>
  </xsl:template>

</xsl:stylesheet>
