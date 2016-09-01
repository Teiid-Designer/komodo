/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import org.komodo.relational.resource.Driver;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a data service entry for a driver file.
 */
public interface DriverEntry extends DataServiceResourceEntry< Driver > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.DRIVER_ENTRY;

    /**
     * An empty collection of driver entries.
     */
    DriverEntry[] NO_ENTRIES = new DriverEntry[ 0 ];

}
