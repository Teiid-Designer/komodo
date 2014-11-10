/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.teiid.query.proc.wsdl;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import org.komodo.spi.query.proc.wsdl.WsdlAttributeInfo;
import org.komodo.spi.query.proc.wsdl.WsdlColumnInfo;
import org.komodo.spi.query.proc.wsdl.WsdlConstants;
import org.komodo.spi.query.proc.wsdl.WsdlRequestInfo;
import org.komodo.spi.query.proc.wsdl.WsdlWrapperInfo;
import org.komodo.spi.query.proc.wsdl.model.Message;
import org.komodo.spi.query.proc.wsdl.model.Part;
import org.komodo.spi.query.sql.SQLConstants;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
public class WsdlRequestProcedureHelper extends AbstractWsdlHelper implements WsdlConstants, SQLConstants {
	
	private final WsdlRequestInfo requestInfo;
    private final Properties properties;
    private final static String NSSTRING = "nsString"; //$NON-NLS-1$
    private final static String NS = "ns"; //$NON-NLS-1$
    private final static String NSPREFIX = "prefix"; //$NON-NLS-1$

    /**
     * @param teiidVersion 
     * @param requestInfo
     * @param properties
     */
    public WsdlRequestProcedureHelper(TeiidVersion teiidVersion, WsdlRequestInfo requestInfo, Properties properties) {
        super(teiidVersion);
        this.requestInfo = requestInfo;
        this.properties = properties;
    }

    private WsdlWrapperInfo getWrapperProcedure() {
        return requestInfo.getWrapperProcedure();
    }
    
    private String getFullParameterName(String procedureName, String name) {
        StringBuilder builder = new StringBuilder();
        builder.append(getWrapperProcedure().getViewModelName());
        builder.append('.').append(procedureName).append('.').append(convertSqlNameSegment(name));
        
        return builder.toString();
    }

    private String getPartElementName(Part part) {
        String partElementName = null;

        partElementName = part.getTypeName();
        if (partElementName == null) {
            partElementName = part.getElementName();
        }

        return partElementName;
    }
    
    private void addAttributesForElement(StringBuffer sb, WsdlColumnInfo columnInfo) {
        if( columnInfo.getAttributeInfoArray().length > 0 ) {
            sb.append(COMMA).append(RETURN).append(TAB).append(TAB).append(TAB).append(TAB).append(XMLATTRIBUTES);
            sb.append(L_PAREN);
            int index = 0;
            for( WsdlAttributeInfo attrInfo : columnInfo.getAttributeInfoArray() ) {
                if( index > 0 ) {
                    sb.append(COMMA).append(SPACE);
                }
                sb.append(attrInfo.getSymbolName());
                if( !attrInfo.getSymbolName().equalsIgnoreCase(attrInfo.getAlias())) {
                    sb.append(SPACE).append(AS).append(SPACE);
                    sb.append(attrInfo.getAlias());
                }
                index++;
            }
            sb.append(R_PAREN);
        }
    }
    
    private String getHeaderString() {
        /* EXAMPLE
        XMLELEMENT(NAME "soapenv:Envelope", 
        XMLNAMESPACES('http://schemas.xmlsoap.org/soap/envelope/' AS soapenv, DEFAULT 'http://soapheader.ibm.com'), 
        XMLELEMENT(NAME "soapenv:Header", 
        XMLELEMENT(NAME quote_timestamp, StockServiceServiceView.getLastSellPrice_request.quote_timestamp)), 
        XMLELEMENT(NAME "soapenv:Body", XMLELEMENT(NAME getLastSellPrice, 
        XMLELEMENT(NAME ticker, StockServiceServiceView.getLastSellPrice_request.ticker)))) AS xml_out;
         */
        
        StringBuilder headerString = new StringBuilder();
        
        if( requestInfo.isMessageServiceMode()) {
            
            //Initial Envelope Element. 
            headerString.append(XMLELEMENT).append(L_PAREN).append(NAME).append(SPACE).append(D_QUOTE).append(ENVELOPE_NAME).append(D_QUOTE).append(COMMA);
            headerString.append(SPACE).append(XMLNAMESPACES).append(L_PAREN).append(S_QUOTE).append(ENVELOPE_NS).append(S_QUOTE).append(SPACE).append(AS).append(SPACE);
            headerString.append(ENVELOPE_NS_ALIAS);
            
            if (requestInfo.getNamespaceMap().size()>0 || (getWrapperProcedure().getNamespaceURI() != null &! getWrapperProcedure().getNamespaceURI().isEmpty())){
            	
            	String prefix=requestInfo.getReverseNSMap().get(getWrapperProcedure().getNamespaceURI());
            	if (isDefaultNS(prefix)) {
            		headerString.append(COMMA).append(SPACE).append(DEFAULT).append(SPACE).append(S_QUOTE).append(getWrapperProcedure().getNamespaceURI());
            		headerString.append(S_QUOTE);
            	}else{
        			headerString.append(COMMA).append(SPACE).append(S_QUOTE).append(getWrapperProcedure().getNamespaceURI()).append(S_QUOTE).append(SPACE).append(AS).append(SPACE);
                    headerString.append(prefix);
        		}
                for (Entry<String, String> entry: requestInfo.getNamespaceMap().entrySet()){
                    //We already added this, so skip it
                    if (entry.getValue().equals(getWrapperProcedure().getNamespaceURI())) break;
                    prefix=requestInfo.getReverseNSMap().get(entry.getValue());
                	if (isDefaultNS(prefix)) {
                		headerString.append(COMMA).append(SPACE).append(DEFAULT).append(SPACE).append(S_QUOTE).append(entry.getValue());
                	}else{
            			headerString.append(COMMA).append(SPACE).append(S_QUOTE).append(entry.getValue()).append(S_QUOTE).append(SPACE).append(AS).append(SPACE);
                        headerString.append(prefix);
            		}
                }
            }
            
            headerString.append(R_PAREN).append(COMMA).append(SPACE).append(XMLELEMENT).append(L_PAREN).append(NAME).append(SPACE);
            headerString.append(D_QUOTE).append(HEADER_NAME).append(D_QUOTE);
        
            for (WsdlColumnInfo columnInfo : requestInfo.getHeaderColumnInfoList()){
                headerString.append(COMMA).append(SPACE).append(XMLELEMENT).append(L_PAREN).append(NAME).append(SPACE).append(columnInfo.getSymbolName());
                headerString.append(COMMA).append(SPACE).append(getFullParameterName(requestInfo.getProcedureName(), columnInfo.getSymbolName()));
                headerString.append(R_PAREN);
            }
            
            headerString.append(R_PAREN).append(COMMA).append(SPACE);
            headerString.append(XMLELEMENT).append(L_PAREN).append(NAME).append(SPACE).append(D_QUOTE).append(BODY_NAME).append(D_QUOTE).append(COMMA);
       }
        
        return headerString.toString();
    }

	/**
	 * @param prefix
	 * @return
	 */
	private boolean isDefaultNS(String prefix) {
		return prefix!=null && prefix.trim().length()==0;
	}
    
    private String getPartElementNamespace(Part part) {
        String partElementNamespace = null;

        partElementNamespace = part.getTypeNamespace();
        if (partElementNamespace == null) {
            partElementNamespace = part.getElementNamespace();
        }

        return partElementNamespace;
    }
    
    private Map<String, String> getNamespaceString(Part[] parts) {
        
    	Map<String, String> nsMap = new HashMap<String, String>();
    	String ns = null;
    	String nsPrefix = null;
    	
        if( getWrapperProcedure().getNamespaceURI() == null ) {
            return nsMap;
        }
        
        if (parts!=null && parts.length==1){
            ns = getPartElementNamespace(parts[0]);
        }
        if (ns==null) ns = getWrapperProcedure().getNamespaceURI();
        StringBuffer sb = new StringBuffer();
        
        nsPrefix=requestInfo.getReverseNSMap().get(ns);
        nsMap.put(NS, ns);
        nsMap.put(NSPREFIX, nsPrefix);
    	if (isDefaultNS(nsPrefix)) {
    		 sb.append(DEFAULT).append(SPACE).append(S_QUOTE).append(ns).append(S_QUOTE);
    	}else{
    		sb.append(XMLNAMESPACES).append(L_PAREN);
			sb.append(SPACE).append(S_QUOTE).append(getWrapperProcedure().getNamespaceURI()).append(S_QUOTE).append(SPACE).append(AS).append(SPACE);
            sb.append(nsPrefix);
		}
        
    	sb.append(R_PAREN);
    	nsMap.put(NSSTRING, sb.toString());
    	
        return nsMap;
    }
    
    /**
     * Generate the SQL statement for this request info
     * 
     * @return SQL string
     */
    public String getSQLStatement() {
//      CREATE VIRTUAL PROCEDURE
//      BEGIN
//          SELECT 
//              XMLELEMENT(NAME "update", XMLNAMESPACES(DEFAULT 'http://quickstart.samples/xsd'), 
//                  XMLELEMENT(NAME symbol, StockQuoteServiceXML."update".create_update_.symbol, 
//                      XMLATTRIBUTES( symbol.empno AS "work_number"), 
//                  XMLELEMENT(NAME price, StockQuoteServiceXML."update".create_update_.price)
//                  ) AS xml_out;
//      END
//      
//      CREATE VIRTUAL PROCEDURErequestInfo.operation.getName()
//      BEGIN
//          SELECT                                 | ------------- element-namespace------------------|
//              XMLELEMENT(NAME "<operation-name>", XMLNAMESPACES(DEFAULT 'http://<schema-namespace>'),
//              | ------------- element-content-expression ----------------------|  
//                  XMLELEMENT(NAME <element-name-1>, <element-content-value>. ...), 
//                  XMLELEMENT(NAME <element-name-2>, <element-content-value>. ...),
//                  ...
//                  ) AS xml_out;
//      END
//  
        
        // Request procedure name may have been overridden
        String requestProcedureName = properties.getProperty(KEY_REQUEST_PROCEDURE_NAME);
        if( requestProcedureName == null ) {
            requestProcedureName = requestInfo.getProcedureName();
        }
        
        String alias = "xml_out"; //$NON-NLS-1$
        StringBuffer sb = new StringBuffer();
        int i=0;
        sb.append(SQL_BEGIN);
        sb.append(TAB).append(SELECT).append(RETURN);
        String header = getHeaderString();
        if( header.length() > 0 ) {
            sb.append(TAB).append(TAB).append(header).append(RETURN);
        }
        sb.append(TAB).append(TAB).append(XMLELEMENT);
        sb.append(L_PAREN);
        
        sb.append(NAME).append(SPACE);
        
        String elementName = null;
        Message message = null;
        Part[] parts = null;
       
            
        if (!(requestInfo.isMessageServiceMode())) {
            message = requestInfo.getOperation().getInputMessage();
            parts = message.getParts();
            if (parts.length>0) {
            	elementName = getPartElementName(parts[0]);
            }
        }
        
        if (elementName == null){
        	elementName = requestInfo.getOperation().getName();
        }
            
        Map<String, String> nsMap = getNamespaceString(parts);
        if( nsMap == null ) { 
        	nsMap = new HashMap<String, String>();
        }
        String nsString = nsMap.get(NSSTRING);
        String nsPrefix = nsMap.get(NSPREFIX);
        if (nsPrefix!=null &! isDefaultNS(nsPrefix)){
        	sb.append("\""+nsPrefix+":"+elementName+"\"").append(COMMA).append(SPACE); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }else{
        	sb.append(convertSqlNameSegment(elementName)).append(COMMA).append(SPACE);
        }
        
        //If this is MESSAGE mode, no need to add NS since we did that at the Envelope level
        if (!(requestInfo.isMessageServiceMode())) {
            if( nsString != null && !nsString.isEmpty() ) {
                sb.append(nsString);
            }
        }
        
        int nColumns = requestInfo.getBodyColumnInfoList().length;
        if( nColumns > 0 ) {
            //If this is MESSAGE mode, we don't need this extra comma
            if (!(requestInfo.isMessageServiceMode())) sb.append(COMMA);
        }
        sb.append(SPACE).append(RETURN);
        // EXAMPLE:       XMLELEMENT(NAME FromCurrency, CurrencyConvertorView.request_ConversionRate.FromCurrency)
        // SELECT XMLELEMENT(NAME GetStateInfo, XMLNAMESPACES(DEFAULT 'http://www.teiid.org/stateService/'), 
        //        XMLELEMENT(NAME stateCode, XMLNAMESPACES(NO DEFAULT), stateServiceView.GetStateInfo_request.stateCode)) AS xml_out;
            
        for( WsdlColumnInfo columnInfo : requestInfo.getBodyColumnInfoList()) {
            String name = columnInfo.getSymbolName();
            String convertedName = convertSqlNameSegment(name);
            Object nsPrefixObject = this.requestInfo.getReverseNSMap().get(columnInfo.getNamespace());
            if (nsPrefixObject!=null) {
            	convertedName = D_QUOTE.concat((String) nsPrefixObject).concat(COLON).concat(convertedName).concat(D_QUOTE);
            }
            sb.append(TAB).append(TAB).append(TAB).append(XMLELEMENT);
            sb.append(L_PAREN);
            sb.append(NAME).append(SPACE).append(convertedName);
            if (columnInfo.getNamespace()==null){
                sb.append(COMMA).append(SPACE).append(XMLNAMESPACES).append(L_PAREN).append(NO_DEFAULT).append(R_PAREN);    
            }
            if( columnInfo.getAttributeInfoArray().length == 0 ) {
                sb.append(COMMA).append(SPACE).append(getFullParameterName(requestProcedureName, name));
            }
                
            addAttributesForElement(sb, columnInfo);
                
            sb.append(R_PAREN);
            if(i < (nColumns-1)) {
                sb.append(COMMA).append(SPACE).append(RETURN);
            }
            i++;
        }
        
        sb.append(R_PAREN);
        //Need two extra parenthesis for MESSAGE mode to wrap everything in the SOAP Envelope and Body
        if (requestInfo.isMessageServiceMode()) sb.append(R_PAREN).append(R_PAREN);
        sb.append(SPACE).append(AS).append(SPACE).append(alias).append(SEMI_COLON);
        sb.append(SQL_END);

        
        return sb.toString();
    }
}
