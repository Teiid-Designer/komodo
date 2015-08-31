/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import org.komodo.rest.json.serialize.LinkSerializer;
import org.komodo.rest.json.serialize.VdbDescriptorSerializer;
import org.komodo.rest.json.serialize.VdbDirectorySerializer;
import org.komodo.rest.json.serialize.VdbEntrySerializer;
import org.komodo.rest.json.serialize.VdbImportSerializer;
import org.komodo.rest.json.serialize.VdbSerializer;
import org.komodo.rest.json.serialize.VdbTranslatorSerializer;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * Identifiers used in Komodo REST object JSON representations.
 */
public interface JsonConstants {

    /**
     * The shared JSON serialier/deserializer for {@link KomodoRestEntity} objects.
     */
    Gson JSON_BUILDER = new GsonBuilder().registerTypeAdapter( RestLink.class, new LinkSerializer() )
                                         .registerTypeAdapter( RestVdb.class, new VdbSerializer() )
                                         .registerTypeAdapter( RestVdbDescriptor.class, new VdbDescriptorSerializer() )
                                         .registerTypeAdapter( RestVdbDirectory.class, new VdbDirectorySerializer() )
                                         .registerTypeAdapter( RestVdbEntry.class, new VdbEntrySerializer() )
                                         .registerTypeAdapter( RestVdbImport.class, new VdbImportSerializer() )
                                         .registerTypeAdapter( RestVdbTranslator.class, new VdbTranslatorSerializer() )
                                         .create();

    /**
     * Value is "{@value}".
     */
    String DESCRIPTION = "description"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String HREF = "href"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String ID = "id"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String IMPORT_DATA_POLICIES = "importDataPolicies"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String LINKS = "links"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String METHOD = "method"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String ORIGINAL_FILE = "originalFile"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String PATH = "path"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String PROPERTIES = "properties"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String REL = "rel"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String TYPE = "type"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String VDBS = "vdbs"; //$NON-NLS-1$

    /**
     * Value is "{@value}".
     */
    String VERSION = "version"; //$NON-NLS-1$

}
