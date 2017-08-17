
<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions" xmlns:xdt="http://www.w3.org/2005/xpath-datatypes"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:jb="urn:jboss:domain:1.4"
	exclude-result-prefixes="xs xsl xsi fn xdt jb cache-container">
	<xsl:output method="xml" version="1.0" encoding="UTF-8"
		indent="yes" />

	<xsl:template match="@*|node()">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" />
		</xsl:copy>
	</xsl:template>
    
    
	<xsl:template match="node()[name(.)='license']">
	   <xsl:variable name="group_id" select="../../groupId" />
	   <xsl:variable name="artifact_id" select="../../artifactId" />
	   
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" />
			<xsl:variable name="filename1" select="concat(../../groupId, ',')" />
			<xsl:variable name="filename2" select="concat($filename1, ../../artifactId)" />
			<xsl:variable name="filename3" select="concat($filename2, ',')" />
			<xsl:variable name="filename4" select="concat($filename3, name)" />
			<local><xsl:value-of select="translate($filename4, ' ', '' )"/></local>
		</xsl:copy>
	</xsl:template>

</xsl:stylesheet>

