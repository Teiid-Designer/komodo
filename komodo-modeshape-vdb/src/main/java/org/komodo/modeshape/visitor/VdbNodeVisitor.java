/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.visitor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.jcr.nodetype.NodeType;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.modeshape.AbstractNodeVisitor;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.vdb.lexicon.CoreLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Visitor that will walk a vdb node tree and convert it to
 * the dynamic vdb xml syntax.
 */
public class VdbNodeVisitor extends AbstractNodeVisitor implements StringConstants {

    /**
     * Companion XML tag for permission condition
     */
    public static String DATA_ROLE_PERMISSION_CONDITION_XML = "condition"; //$NON-NLS-1$

    private static final String UNDEFINED = null;

    private static Map<String, NodeTypeName> nodeNameIndex = new HashMap<String, NodeTypeName>();

    private enum NodeTypeName {

        VIRTUAL_DATABASE(VdbLexicon.Vdb.VIRTUAL_DATABASE, VdbLexicon.ManifestIds.VDB),

        DESCRIPTION(VdbLexicon.Vdb.DESCRIPTION, VdbLexicon.ManifestIds.DESCRIPTION),

        IMPORT_VDBS(VdbLexicon.Vdb.IMPORT_VDBS),

        IMPORT_VDB(VdbLexicon.ImportVdb.IMPORT_VDB, VdbLexicon.ManifestIds.IMPORT_VDB),

        MODEL(VdbLexicon.Vdb.DECLARATIVE_MODEL, VdbLexicon.ManifestIds.MODEL),

        SOURCES(VdbLexicon.Vdb.SOURCES),

        SOURCE(VdbLexicon.Source.SOURCE, VdbLexicon.ManifestIds.SOURCE),

        TRANSLATORS(VdbLexicon.Vdb.TRANSLATORS),

        TRANSLATOR(VdbLexicon.Translator.TRANSLATOR, VdbLexicon.ManifestIds.TRANSLATOR),

        DATA_ROLES(VdbLexicon.Vdb.DATA_ROLES),

        DATA_ROLE(VdbLexicon.DataRole.DATA_ROLE, VdbLexicon.ManifestIds.DATA_ROLE),

        PERMISSIONS(VdbLexicon.DataRole.PERMISSIONS),

        PERMISSION(VdbLexicon.DataRole.Permission.PERMISSION, VdbLexicon.ManifestIds.PERMISSION),

        CONDITIONS(VdbLexicon.DataRole.Permission.CONDITIONS),

        CONDITION(VdbLexicon.DataRole.Permission.Condition.CONDITION, VdbLexicon.ManifestIds.CONDITION),

        MASKS(VdbLexicon.DataRole.Permission.MASKS),

        MASK(VdbLexicon.DataRole.Permission.Mask.MASK, VdbLexicon.ManifestIds.MASK),

        UNKNOWN(UNDEFINED);

        private final String id;

        private final String tag;

        private NodeTypeName(String id, String tag) {
            this.id = id;
            this.tag = tag;
            nodeNameIndex.put(this.id, this);
        }

        private NodeTypeName(String name) {
            this(name, UNDEFINED);
        }

        /**
         * @return the name
         */
        public String getId() {
            return this.id;
        }

        /**
         * @return the tag
         */
        public String getTag() {
            return this.tag;
        }

        /**
         * @param name name value
         * @return the name enum for the given name
         */
        public static NodeTypeName findName(String name) {
            NodeTypeName ntName = nodeNameIndex.get(name);
            if (ntName != null)
                return ntName;

            return UNKNOWN;
        }
    }

    @SuppressWarnings( "unused" )
    private interface ElementTabValue {
        int VIRTUAL_DATABASE = 0;
        int VDB_PROPERTY = 1;
        int DESCRIPTION = 1;
        int CONNECTION_TYPE = 1;
        int IMPORT_VDB = 1;

        int MODEL = 1;
        int MODEL_PROPERTY = 2;
        int MODEL_DESCRIPTION = 2;
        int MODEL_METADATA = 2;
        int MODEL_VALIDATION = 2;
        int MODEL_SOURCE = 2;

        int TRANSLATOR = 1;
        int TRANSLATOR_PROPERTY = 2;

        int DATA_ROLE = 1;
        int DATA_ROLE_DESCRIPTION = 2;
        int PERMISSION = 2;
        int MAPPED_ROLE_NAME = 2;
        int RESOURCE_NAME = 3;
        int PERMISSION_ALLOW = 3;
        int CONDITION = 3;
        int MASK = 3;

        int ENTRY = 1;
        int ENTRY_PROPERTY = 2;
        int ENTRY_DESCRIPTION = 2;
    }

    private final XMLStreamWriter writer;

    private boolean showTabs;

    /**
     * Create new visitor that writes to the given xml stream writer
     *
     * @param version teiid version
     * @param writer output for the xml
     */
    public VdbNodeVisitor(TeiidVersion version, XMLStreamWriter writer) {
        super(version);
        this.writer = writer;
    }

    @Override
    protected String undefined() {
        return UNDEFINED;
    }

    /**
     * Set to true to show tabs
     *
     * @param showTabs showTabs flags
     */
    public void setShowTabs(boolean showTabs) {
    	this.showTabs = showTabs;
    }

    private void writeNewLine(int total) throws XMLStreamException {
        for (int i = 0; i < total; ++i)
            writer.writeCharacters(NEW_LINE);
    }

    private void writeNewLine() throws XMLStreamException {
        writeNewLine(1);
    }

    private void writeTab(int total) throws XMLStreamException {
    	if( showTabs ) {
    		for (int i = 0; i < total; ++i)
    			writer.writeCharacters(TAB);
    	}
    }

    private void writeStartDocument() throws XMLStreamException {
        writer.writeStartDocument("UTF-8", "1.0"); //$NON-NLS-1$ //$NON-NLS-2$
        writeNewLine();
    }

    private void writeStartElement(String tag) throws XMLStreamException {
        writer.writeStartElement(tag);
    }

    private void writeAttribute(String name, String value) throws XMLStreamException {
    	if(value!=null) writer.writeAttribute(name, value);
    }

    private void writeCData(String data) throws XMLStreamException {
        writer.writeCData(data);
    }

    private void writeCharacters(String characters) throws XMLStreamException {
        writer.writeCharacters(characters);
    }

    private void writeEndElement() throws XMLStreamException {
        writer.writeEndElement();
        writeNewLine();
    }

    private void writeElementWithText(String name, String text) throws XMLStreamException {
        writeStartElement(name);
        writeCharacters(text);
        writeEndElement();
    }

    private void writeEndDocument() throws XMLStreamException {
        writer.writeEndDocument();
        writer.close();
    }

    private boolean isPrimaryNodeType(Node node, NodeTypeName nodeTypeName) throws RepositoryException {
        NodeType nodeType = node.getPrimaryNodeType();
        return nodeTypeName.getId().equals(nodeType.getName());
    }

    private void properties(Node node, int numTabs, Properties exportableProps) throws XMLStreamException {

        for( Object key : exportableProps.keySet() ) {
        	String name = (String)key;
            String value = exportableProps.getProperty(name);

            writeTab(numTabs);
            writeStartElement(VdbLexicon.ManifestIds.PROPERTY);
            writeAttribute(VdbLexicon.ManifestIds.NAME, name);
            writeAttribute(VdbLexicon.ManifestIds.VALUE, value);
            writeEndElement();
        }
    }

    private void mask(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.MASK))
            return;

        writeTab(ElementTabValue.MASK);
        // Condition element
        writeStartElement(NodeTypeName.MASK.getTag());

        Property property = property(node, VdbLexicon.DataRole.Permission.Mask.ORDER);
        writeAttribute(VdbLexicon.ManifestIds.ORDER, toString(property));

        writeCharacters(node.getName());
        writeEndElement();
    }

    private void condition(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.CONDITION))
            return;

        // Condition element
        writeTab(ElementTabValue.CONDITION);
        writeStartElement(NodeTypeName.CONDITION.getTag());

        Property property = property(node, VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);
        writeAttribute(VdbLexicon.ManifestIds.CONSTRAINT, toString(property));

        writeCharacters(node.getName());
        writeEndElement();
    }

    private void permission(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.PERMISSION))
            return;

        // Permission element
//        writeNewLine();
        writeTab(ElementTabValue.PERMISSION);
        writeStartElement(NodeTypeName.PERMISSION.getTag());

        // Resource name element
        writeNewLine();
        writeTab(ElementTabValue.RESOURCE_NAME);
        writeElementWithText(VdbLexicon.ManifestIds.RESOURCE_NAME, node.getName());

        String[][] permTags = {
            { VdbLexicon.DataRole.Permission.ALLOW_ALTER, VdbLexicon.ManifestIds.ALLOW_ALTER },
            { VdbLexicon.DataRole.Permission.ALLOW_CREATE, VdbLexicon.ManifestIds.ALLOW_CREATE },
            { VdbLexicon.DataRole.Permission.ALLOW_DELETE, VdbLexicon.ManifestIds.ALLOW_DELETE },
            { VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, VdbLexicon.ManifestIds.ALLOW_EXECUTE },
            { VdbLexicon.DataRole.Permission.ALLOW_READ, VdbLexicon.ManifestIds.ALLOW_READ },
            { VdbLexicon.DataRole.Permission.ALLOW_UPDATE, VdbLexicon.ManifestIds.ALLOW_UPDATE },
            { VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, VdbLexicon.ManifestIds.ALLOW_LANGUAGE }
        };

        for (int i = 0; i < permTags.length; ++i) {
            Property permProp = property(node, permTags[i][0]);

            // Don't include allow-language if not present or set to false as when this was present queries were not working
            // in the default read-only data role. Might need a 3-state value (true, false, not set) for this property.
            // See TEIIDTOOLS-224
            if ( VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE.equals( permTags[ i ][ 0 ] )
                 && ( ( permProp == null ) || !permProp.getBoolean() ) ) {
                continue;
            }

            Boolean value = permProp == null ? false : permProp.getBoolean();
            writeTab(ElementTabValue.PERMISSION_ALLOW);
            writeElementWithText(permTags[i][1], value.toString());
        }

        // Conditions
        visitChild(node, NodeTypeName.CONDITIONS.getId());

        // Masks
        visitChild(node, NodeTypeName.MASKS.getId());

        // End Permission
        writeTab(ElementTabValue.PERMISSION);
        writeEndElement();
    }

    private void dataRole(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.DATA_ROLE))
            return;

        // Data Role element
        writeTab(ElementTabValue.DATA_ROLE);
        writeStartElement(NodeTypeName.DATA_ROLE.getTag());

        // Process data role attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property authProp = property(node, VdbLexicon.DataRole.ANY_AUTHENTICATED);
        writeAttribute(VdbLexicon.ManifestIds.ANY_AUTHENTICATED, toString(authProp));
        Property tempTablesProp = property(node, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);
        writeAttribute(VdbLexicon.ManifestIds.ALLOW_CREATE_TEMP_TABLES, toString(tempTablesProp));
        Property grantAllProp = property(node, VdbLexicon.DataRole.GRANT_ALL);
        writeAttribute(VdbLexicon.ManifestIds.GRANT_ALL, toString(grantAllProp));

        writeNewLine();

        description(node, ElementTabValue.DATA_ROLE_DESCRIPTION);

        // Permissions
        visitChild(node, NodeTypeName.PERMISSIONS.getId());

        // Mapped Role Names
        if ( node.hasProperty( VdbLexicon.DataRole.MAPPED_ROLE_NAMES ) ) {
            Property property = node.getProperty(VdbLexicon.DataRole.MAPPED_ROLE_NAMES);
            Value[] mappedRoleValues = property.getValues();
            for (Value value : mappedRoleValues) {
                writeTab(ElementTabValue.MAPPED_ROLE_NAME);
                writeElementWithText(VdbLexicon.ManifestIds.MAPPED_ROLE_NAME, value.getString());
            }
        }
        
        writeTab(ElementTabValue.DATA_ROLE);
        writeEndElement();
    }

    private void translator(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.TRANSLATOR))
            return;

        // Translator element
        writeTab(ElementTabValue.TRANSLATOR);
        writeStartElement(NodeTypeName.TRANSLATOR.getTag());

        // Process translator attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property typeProp = property(node, VdbLexicon.Translator.TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(typeProp));
        Property descProp = property(node, VdbLexicon.Translator.DESCRIPTION);
        writeAttribute(VdbLexicon.ManifestIds.DESCRIPTION, toString(descProp));

        writeNewLine();

        // Process property attributes
        Properties exportableProps = filterExportableProperties(node.getProperties(), VdbLexicon.Translator.TYPE, VdbLexicon.Translator.DESCRIPTION);
        properties(node, ElementTabValue.TRANSLATOR_PROPERTY, exportableProps);

        writeTab(ElementTabValue.TRANSLATOR);
        writeEndElement();
    }

    private void source(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.SOURCE))
            return;

        // Translator element
        writeTab(ElementTabValue.MODEL_SOURCE);
        writeStartElement(NodeTypeName.SOURCE.getTag());

        // Process source attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property translatorProp = property(node, VdbLexicon.Source.TRANSLATOR);
        writeAttribute(VdbLexicon.ManifestIds.TRANSLATOR_NAME, toString(translatorProp));
        Property jndiProp = property(node, VdbLexicon.Source.JNDI_NAME);
        writeAttribute(VdbLexicon.ManifestIds.JNDI_NAME, toString(jndiProp));

        writeEndElement();
    }

    private void model(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.MODEL))
            return;

        writeTab(ElementTabValue.MODEL);
        writeStartElement(NodeTypeName.MODEL.getTag());

        writeAttribute(VdbLexicon.ManifestIds.NAME, node.getName());

        Property typeProp = property(node, CoreLexicon.JcrId.MODEL_TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(typeProp));

        Property pathProp = property(node, VdbLexicon.Model.PATH_IN_VDB);
        if (pathProp != null)
            writeAttribute(VdbLexicon.ManifestIds.PATH, toString(pathProp));

        Property visibleProp = property(node, VdbLexicon.Model.VISIBLE);
        if (visibleProp != null) {
            String value = toString(visibleProp);
            if (! Boolean.parseBoolean(value)) // True is the default value so no need to include if true
                writeAttribute(VdbLexicon.ManifestIds.VISIBLE, value);
        }

        writeNewLine();
        description(node, ElementTabValue.MODEL_DESCRIPTION);

        Properties exportableProps = filterExportableProperties(node.getProperties(), CoreLexicon.JcrId.MODEL_TYPE);

        properties(node, ElementTabValue.MODEL_PROPERTY, exportableProps);

        // Sources
        visitChild(node, NodeTypeName.SOURCES.getId());

        DdlNodeVisitor visitor = new DdlNodeVisitor(getVersion(), showTabs);
        visitor.visit(node);

        if (! visitor.getDdl().isEmpty()) {
        	writeTab(ElementTabValue.MODEL_METADATA);
            writeStartElement(VdbLexicon.ManifestIds.METADATA);
            Property metaTypeProp = property(node, VdbLexicon.Model.METADATA_TYPE);
            writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(metaTypeProp));

        	writeNewLine();
        	writeTab(ElementTabValue.MODEL_METADATA + 1);
            writeCData(visitor.getDdl());

            // end metadata tag
        	writeNewLine();
        	writeTab(ElementTabValue.MODEL_METADATA + 1);
            writeNewLine();
            writeTab(ElementTabValue.MODEL_METADATA);
            writeEndElement();
        }

        // End model tag
        writeTab(ElementTabValue.MODEL);
        writeEndElement();
    }

    private void importVdb(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.IMPORT_VDB))
            return;

        // Import-vdb element
        writeTab(ElementTabValue.IMPORT_VDB);
        writeStartElement(NodeTypeName.IMPORT_VDB.getTag());

        // Process import-vdb attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property versionProp = property(node, VdbLexicon.ImportVdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(versionProp));
        Property dataPoliciesProp = property(node, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);
        if (dataPoliciesProp != null)
            writeAttribute(VdbLexicon.ManifestIds.IMPORT_DATA_POLICIES, toString(dataPoliciesProp));

        writeTab(ElementTabValue.IMPORT_VDB);
        writeEndElement();
    }

    private void description(Node node, int numTabs) throws XMLStreamException, RepositoryException {
        Property property;
        property = property(node, NodeTypeName.DESCRIPTION.getId());
        if (property == null)
            return;

        writeTab(numTabs);
        writeElementWithText(NodeTypeName.DESCRIPTION.getTag(), toString(property));
    }

    private Properties filterExportableProperties(PropertyIterator propIter, String... propertiesToIgnore) throws RepositoryException {
        Properties exportableProps = new Properties();
        List<String> propsToIgnore = Arrays.asList(propertiesToIgnore);

        while(propIter.hasNext()) {
            Property property = propIter.nextProperty();
            String name = property.getName();
            if (name == null)
                continue;

            if (propsToIgnore.contains(name))
                continue;

            // Ignore jcr properties since these are internal to modeshape
            if (name.startsWith(JcrLexicon.Namespace.PREFIX))
                continue;


            String value = toString(property);

            //
            // Ignore modeshape vdb properties as <property> type properties will
            // not have a vdb prefix but simply be the property name on its own, eg.
            // UseConnectedMetadata or vdb-property1.
            //
            if (name.startsWith(VdbLexicon.Namespace.PREFIX + COLON)) {
                //
                // Preview is actually converted into a vdb property so need to special-case
                // turn it back into a simple property name but we only care if the property
                // is actually true.
                //
                if (name.equals(VdbLexicon.Vdb.PREVIEW) && Boolean.parseBoolean(value)) {
                    name = VdbLexicon.ManifestIds.PREVIEW;
                } else {
                	continue;
                }
            }

            name = convertNamePrefixToUri( property );
            exportableProps.put(name, value);
        }

        return exportableProps;
    }

    private String convertNamePrefixToUri( final Property property ) throws RepositoryException {
        final String name = property.getName();
        final int index = name.indexOf( COLON );

        // if JCR expanded name or just a local name just return the name
        if ( index == -1 ) {
            return name;
        }

        // convert JCR qualified name to expanded name
        final String prefix = name.substring( 0, index );
        final String uri = property.getSession().getNamespaceURI( prefix );
        final QName expanded = new QName( uri, name.substring( index + 1 ) );

        return expanded.toString();
    }

    private void virtualDatabase(Node node) throws XMLStreamException, RepositoryException {
        // Start new document
        writeStartDocument();

        // Vdb element
        writeTab(ElementTabValue.VIRTUAL_DATABASE);
        writeStartElement(NodeTypeName.VIRTUAL_DATABASE.getTag());

        // Name attribute
        Property property = property(node, VdbLexicon.Vdb.NAME);
        writeAttribute(VdbLexicon.ManifestIds.NAME, toString(property));

        // Version attribute
        property = property(node, VdbLexicon.Vdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(property));

        writeNewLine(2);

        // Description element
        description(node, ElementTabValue.DESCRIPTION);

        // Connection Type element
        property = property(node, VdbLexicon.Vdb.CONNECTION_TYPE);
        if (property != null) {
        	writeTab(ElementTabValue.CONNECTION_TYPE);
            writeElementWithText(VdbLexicon.ManifestIds.CONNECTION_TYPE, toString(property));
        }

        // Properties elements
        Properties exportableProps = filterExportableProperties(node.getProperties(),
        		VdbLexicon.Vdb.NAME, VdbLexicon.Vdb.VERSION,
                NodeTypeName.DESCRIPTION.getId(), VdbLexicon.Vdb.CONNECTION_TYPE, JcrConstants.MODE_SHA1);

        properties(node, ElementTabValue.VDB_PROPERTY, exportableProps);

        writeNewLine();

        //
        // Visit vdb children by name since the xsd demands them in a specific order
        //

        // Import Vdbs
        visitChild(node, NodeTypeName.IMPORT_VDBS.getId());

        // Models
        visitFilteredChildren(node, NodeTypeName.MODEL.getId());

        // Translators
        visitChild(node, NodeTypeName.TRANSLATORS.getId());

        // Data Roles
        visitChild(node, NodeTypeName.DATA_ROLES.getId());

        writeNewLine();

        // Close out the xml document
        writeTab(ElementTabValue.VIRTUAL_DATABASE);
        writeEndElement();
        writeEndDocument();
    }

    @Override
    public void visit(Node node) throws RepositoryException {
        if (node == null)
            return;

        NodeType nodeType = node.getPrimaryNodeType();
        String nodeTypeName = nodeType.getName();
        NodeTypeName ntName = NodeTypeName.findName(nodeTypeName);
        try {
            switch (ntName) {
                case VIRTUAL_DATABASE:
                    virtualDatabase(node);
                    break;
                case IMPORT_VDBS:
                case TRANSLATORS:
                case SOURCES:
                case DATA_ROLES:
                case PERMISSIONS:
                case CONDITIONS:
                case MASKS:
                    visitChildren(node);
                    break;
                case MODEL:
                    model(node);
                    break;
                case IMPORT_VDB:
                    importVdb(node);
                    break;
                case TRANSLATOR:
                    translator(node);
                    break;
                case SOURCE:
                    source(node);
                    break;
                case DATA_ROLE:
                    dataRole(node);
                    break;
                case PERMISSION:
                    permission(node);
                    break;
                case CONDITION:
                    condition(node);
                    break;
                case MASK:
                    mask(node);
                    break;
                case UNKNOWN:
                default:
                    // Not a node we are interested in but may contain such nodes
                    visitChildren(node);
            }
        } catch (XMLStreamException ex) {
            throw new RepositoryException(ex);
        }
    }


    @Override
    public void visit(Property property) {
        // Not required
    }

    @Override
    protected void visitChild(Node node, String relNodePath) throws PathNotFoundException, RepositoryException {
        try {
        	if (node.hasNode(relNodePath)) {
        		// write tab value based on node path/type
	        	writeTab(getTabValue(relNodePath));

	            Node child = node.getNode(relNodePath);
	            child.accept(this);
        	}
    	} catch (XMLStreamException ex) {
            throw new RepositoryException(ex);
        }
    }

    private int getTabValue(String relNodePath) {
    	if( relNodePath.equals(NodeTypeName.IMPORT_VDB.getId()) ) {
    		return ElementTabValue.IMPORT_VDB;
    	} else     	if( relNodePath.equals(NodeTypeName.CONDITION.getId()) ) {
    		return ElementTabValue.CONDITION;
    	} else     	if( relNodePath.equals(NodeTypeName.DATA_ROLE.getId()) ) {
    		return ElementTabValue.DATA_ROLE;
    	} else     	if( relNodePath.equals(NodeTypeName.DESCRIPTION.getId()) ) {
    		return ElementTabValue.DESCRIPTION;
    	} else     	if( relNodePath.equals(NodeTypeName.MASK.getId()) ) {
    		return ElementTabValue.MASK;
    	} else     	if( relNodePath.equals(NodeTypeName.MODEL.getId()) ) {
    		return ElementTabValue.MODEL;
    	} else     	if( relNodePath.equals(NodeTypeName.PERMISSION.getId()) ) {
    		return ElementTabValue.PERMISSION;
    	} else     	if( relNodePath.equals(NodeTypeName.SOURCE.getId()) ) {
    		return ElementTabValue.MODEL_SOURCE;
    	} else     	if( relNodePath.equals(NodeTypeName.TRANSLATOR.getId()) ) {
    		return ElementTabValue.TRANSLATOR;
    	} else     	if( relNodePath.equals(NodeTypeName.VIRTUAL_DATABASE.getId()) ) {
    		return ElementTabValue.VIRTUAL_DATABASE;
    	}

    	return 0;
    }
}
