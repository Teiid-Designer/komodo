/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.constants;

/**
 * Constants associated with the Komodo system and environment.
 */
public interface SystemConstants {

    /**
     * The environmental variable that can be set with the directory the Komodo engine will use while running. Default is
     * <code>${user.home}/.komodo</code>.
     */
    String ENGINE_DATA_DIR = "komodo.dataDir"; //$NON-NLS-1$

    /**
     * The Komodo log file name.
     */
    String LOG_FILE_NAME = StringConstants.KOMODO + StringConstants.DOT + StringConstants.LOG;

    /**
     * The environmental variable that can be set with the directory VDB Builder will use while running. Default is
     * <code>${user.home}/.komodo/vdbbuilder</code>
     */
    String VDB_BUILDER_DATA_DIR = "vdbbuilder.dataDir"; //$NON-NLS-1$

}
