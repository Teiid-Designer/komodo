/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import org.komodo.relational.resource.DdlFile;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a data service entry for a DDL file.
 */
public interface DdlEntry extends DataServiceResourceEntry< DdlFile > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.DDL_ENTRY;

    /**
     * An empty collection of DDL entries.
     */
    DdlEntry[] NO_ENTRIES = new DdlEntry[ 0 ];

}
