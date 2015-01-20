/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.core;

import org.komodo.spi.constants.StringConstants;
import org.modeshape.jcr.JcrNtLexicon;

/**
 * Constants for the JCR names of node types and properties related to the Komodo engine.
 */
public interface KomodoLexicon extends StringConstants {

    /**
     * The JCR names associated with a data source node type.
     */
    public interface DataSource extends LibraryComponent, WorkspaceItem {

        /**
         * The node type name of a data source. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "dataSource"; //$NON-NLS-1$

        /**
         * The name and node type name of the data sources grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "dataSources"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with the Komodo node type.
     */
    public interface Komodo {

        /**
         * The name and node type name of the Komodo library node. Value is {@value} .
         */
        String LIBRARY = Namespace.PREFIX + COLON + "library"; //$NON-NLS-1$

        /**
         * The name and node type name of the Komodo node. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "komodo"; //$NON-NLS-1$

        /**
         * The name and node type name of the Komodo workspace node. Value is {@value} .
         */
        String WORKSPACE = Namespace.PREFIX + COLON + "workspace"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with the Komodo library node type.
     */
    public interface Library {

        /**
         * The name and node type name of the Komodo library's data source grouping node. Value is {@value} .
         */
        String DATA_SOURCES = DataSource.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library node. Value is {@value} .
         */
        String NODE_TYPE = Komodo.LIBRARY;

        /**
         * The name and node type name of the Komodo library's schema grouping node. Value is {@value} .
         */
        String SCHEMAS = Schema.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's file entry grouping node. Value is {@value} .
         */
        String VDB_ENTRIES = VdbEntry.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's Import VDB grouping node. Value is {@value} .
         */
        String VDB_IMPORTS = VdbImport.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's VDB manifest model source grouping node. Value is {@value} .
         */
        String VDB_MODEL_SOURCES = VdbModelSource.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's VDB manifest model grouping node. Value is {@value} .
         */
        String VDB_MODELS = VdbModel.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's translator grouping node. Value is {@value} .
         */
        String VDB_TRANSLATORS = VdbTranslator.GROUP_NODE;

        /**
         * The name and node type name of the Komodo library's VDB grouping node. Value is {@value} .
         */
        String VDBS = Vdb.GROUP_NODE;

    }

    /**
     * The JCR names associated with the library component mixin. Library nodes are versionable and referenceable.
     */
    public interface LibraryComponent {

        /**
         * The name of the description property of a library component. Value is {@value} .
         */
        String DESCRIPTION = Namespace.PREFIX + COLON + "description"; //$NON-NLS-1$

        /**
         * The name of the library component mixin. Value is {@value} .
         */
        String MIXIN_TYPE = Namespace.PREFIX + COLON + "libraryComponent"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with the Komodo namespace.
     */
    public interface Namespace {

        /**
         * The Komodo namespace prefix. Value is {@value} .
         */
        String PREFIX = "tko"; //$NON-NLS-1$

        /**
         * The Komodo namespace URI. Value is {@value} .
         */
        String URI = "http://www.teiid.org/komodo/1.0"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with repositories.
     */
    public interface Repository extends WorkspaceItem {

        /**
         * The name of the <code>nt:address</code> node type for the child node. Value is {@value} .
         */
        String ADDRESS = "nt:address"; //$NON-NLS-1$

        /**
         * The name and node type name of the repositories grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "repositories"; //$NON-NLS-1$

        /**
         * The name of the repository node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "repository"; //$NON-NLS-1$

        /**
         * The name of the type property. Value is {@value} .
         */
        String TYPE = Namespace.PREFIX + COLON + "type"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with schemas.
     */
    public interface Schema extends LibraryComponent, WorkspaceItem {

        /**
         * The name of the property used for the external file location. Value is {@value} .
         */
        String EXTERNAL_LOCATION = Namespace.PREFIX + COLON + "externalLocation"; //$NON-NLS-1$

        /**
         * The name and node type name of the Komodo library and workspace's Teiid server grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "schemas"; //$NON-NLS-1$

        /**
         * The name of the rendition property. Value is {@value} .
         */
        String RENDITION = Namespace.PREFIX + COLON + "rendition"; //$NON-NLS-1$

        /**
         * The name of the language object node type for child node(s). Value is {@value} .
         */
        String LANGUAGE_OBJECT = "tsql:languageObject"; //$NON-NLS-1$

        /**
         * The name of the schema node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "schema"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with Teiid servers.
     */
    public interface Teiid extends WorkspaceItem {

        /**
         * The name of the administrator port property. Value is {@value} .
         */
        String ADMIN_PORT = Namespace.PREFIX + COLON + "adminPort"; //$NON-NLS-1$

        /**
         * The name of the administrator password property. Value is {@value} .
         */
        String ADMIN_PSWD = Namespace.PREFIX + COLON + "adminPswd"; //$NON-NLS-1$

        /**
         * The name of the administrator user name property. Value is {@value} .
         */
        String ADMIN_USER = Namespace.PREFIX + COLON + "adminUser"; //$NON-NLS-1$

        /**
         * The name of the connected property. Value is {@value} .
         */
        String CONNECTED = Namespace.PREFIX + COLON + "connected"; //$NON-NLS-1$

        /**
         * The name and node type name of the Komodo workspace's Teiid server grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "teiids"; //$NON-NLS-1$

        /**
         * The name of the host URL property. Value is {@value} .
         */
        String HOST = Namespace.PREFIX + COLON + "host"; //$NON-NLS-1$

        /**
         * The name of the JDBC port property. Value is {@value} .
         */
        String JDBC_PORT = Namespace.PREFIX + COLON + "jdbcPort"; //$NON-NLS-1$

        /**
         * The name of the JDBC password property. Value is {@value} .
         */
        String JDBC_PSWD = Namespace.PREFIX + COLON + "jdbcPswd"; //$NON-NLS-1$

        /**
         * The name of the JDBC user name property. Value is {@value} .
         */
        String JDBC_USER = Namespace.PREFIX + COLON + "jdbcUser"; //$NON-NLS-1$

        /**
         * The name of the last ping time property. Value is {@value} .
         */
        String LAST_PING_TIME = Namespace.PREFIX + COLON + "lastPingTime"; //$NON-NLS-1$

        /**
         * The name of the Teiid node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "teiid"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDBs.
     */
    public interface Vdb extends LibraryComponent, WorkspaceItem {

        /**
         * The name and node type name of the Komodo workspace and library's VDB grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbs"; //$NON-NLS-1$

        /**
         * The name of the VDB node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdb"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDB manifest entries.
     */
    public interface VdbEntry extends LibraryComponent {

        /**
         * The name and node type name of the Komodo library's VDB manifest entry grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbEntries"; //$NON-NLS-1$

        /**
         * The name of the VDB manifest entry node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdbEntry"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDB manifest Import VDBs.
     */
    public interface VdbImport extends LibraryComponent {

        /**
         * The name and node type name of the Komodo library's VDB manifest Import VDB grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbImports"; //$NON-NLS-1$

        /**
         * The name of the VDB manifest entry node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdbImport"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDB manifest models.
     */
    public interface VdbModel extends LibraryComponent {

        /**
         * The node type name of the model's file child node. Value is {@value} .
         */
        String FILE = JcrNtLexicon.FILE.getString();

        /**
         * The name of the metadata property (e.g., DDL). Value is {@value} .
         */

        /**
         * The name and node type name of the Komodo library's VDB manifest model grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbModels"; //$NON-NLS-1$

        /**
         * The name of the metadata property (e.g., DDL). Value is {@value} .
         */
        String METADATA_TYPE = "vdb:metadataType"; //$NON-NLS-1$

        /**
         * The name of the model definition property. This is the code, like DDL, that defines the schema/model. Value is {@value}
         * .
         */
        String MODEL_DEFINITION = "vdb:modelDefinition"; //$NON-NLS-1$

        /**
         * The node type name of the model's source child node(s). Value is {@value} .
         */
        String MODEL_SOURCE = VdbModelSource.NODE_TYPE;

        /**
         * The name of the VDB manifest model node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdbModel"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDB manifest model sources.
     */
    public interface VdbModelSource extends LibraryComponent {

        /**
         * The name and node type name of the Komodo library's VDB manifest model source grouping node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbModelSources"; //$NON-NLS-1$

        /**
         * The name of the JNDI property. Value is {@value} .
         */
        String JNDI_NAME = "vdb:sourceJndiName"; //$NON-NLS-1$

        /**
         * The name of the VDB manifest model source node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdbModelSource"; //$NON-NLS-1$

        /**
         * The name of the translator property. Value is {@value} .
         */
        String TRANSLATOR = "vdb:sourceTranslator"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with VDB manifest translators.
     */
    public interface VdbTranslator extends LibraryComponent {

        /**
         * The name and node type name of the Komodo library's VDB manifest translator grouping chld node. Value is {@value} .
         */
        String GROUP_NODE = Namespace.PREFIX + COLON + "vdbTranslators"; //$NON-NLS-1$

        /**
         * The name of the VDB manifest translator node type. Value is {@value} .
         */
        String NODE_TYPE = Namespace.PREFIX + COLON + "vdbTranslator"; //$NON-NLS-1$

    }

    /**
     * The JCR names associated with the Komodo workspace node type.
     */
    public interface Workspace {

        /**
         * The name and node type name of the Komodo workspace's data source grouping node. Value is {@value} .
         */
        String DATA_SOURCES = DataSource.GROUP_NODE;

        /**
         * The name and node type name of the Komodo workspace node. Value is {@value} .
         */
        String NODE_TYPE = Komodo.WORKSPACE;

        /**
         * The name and node type name of the Komodo workspace's repository grouping node. Value is {@value} .
         */
        String REPOSITORIES = Repository.GROUP_NODE;

        /**
         * The name and node type name of the Komodo workspace's Teiid grouping node. Value is {@value} .
         */
        String TEIIDS = Teiid.GROUP_NODE;

    }

    /**
     * The JCR names associated with the workspace item mixin. Workspace item nodes are referenceable.
     */
    public interface WorkspaceItem {

        /**
         * The name of the workspace item mixin. Value is {@value} .
         */
        String MIXIN_TYPE = Namespace.PREFIX + COLON + "workspaceItem"; //$NON-NLS-1$

        /**
         * The name of the external file location property. Value is {@value} .
         */
        String EXT_LOC = Namespace.PREFIX + COLON + "externalLocation"; //$NON-NLS-1$

        /**
         * The node type name of the file child node. Value is {@value} .
         */
        String FILE_NODE_TYPE = JcrNtLexicon.FILE.getString();

        /**
         * The node name of the child node that contains the original imported resource. Value is {@value} .
         */
        String ORIGINAL_FILE = Namespace.PREFIX + COLON + "originalFile"; //$NON-NLS-1$

    }

}
