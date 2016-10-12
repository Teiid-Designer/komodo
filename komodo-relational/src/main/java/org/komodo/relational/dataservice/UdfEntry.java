/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import org.komodo.relational.resource.UdfFile;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a data service entry for a UDF file.
 */
public interface UdfEntry extends DataServiceResourceEntry< UdfFile > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.UDF_ENTRY;

    /**
     * An empty collection of UDF entries.
     */
    UdfEntry[] NO_ENTRIES = new UdfEntry[ 0 ];

}
