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
package org.komodo.spi.uuid;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.UUID;

/**
 *
 */
public class WorkspaceUUIDService {

    private static WorkspaceUUIDService instance;

    /**
     * Get the singleton instance
     * 
     * @return the single {@link WorkspaceUUIDService}
     */
    public static WorkspaceUUIDService getInstance() {
        if (instance == null) instance = new WorkspaceUUIDService();

        return instance;
    }

    private File restoreFile;

    private WorkspaceUUIDService() {
    }

    /**
     * Set the restore location
     *
     * @param file location for restoration
     */
    public void setRestoreLocation(File file) {
        restoreFile = file;
    }

    /**
     * Get the Workspace's UUID
     * 
     * @return UUID
     */
    public UUID getUUID() {
        UUID workspaceUuid = null;

            if (restoreFile != null && restoreFile.exists()) {
                BufferedReader reader = null;
                try {
                    reader = new BufferedReader(new FileReader(restoreFile));
                    workspaceUuid = UUID.fromString(reader.readLine());
                } catch (final IOException error) {
                    // ignored
                } finally {
                    try {
                        if (reader != null)
                            reader.close();
                    } catch (final IOException ignored) {
                        // ignored
                    }
                }
            }

            if (workspaceUuid == null)
                workspaceUuid = UUID.randomUUID();

            writeUUID(workspaceUuid);
            

        return workspaceUuid;
    }

    /**
     * @param workspaceUuid
     */
    private void writeUUID(UUID workspaceUuid) {
        if (restoreFile == null)
            return;
        
        FileWriter writer = null;
        try {
            writer = new FileWriter(restoreFile);
            writer.write(workspaceUuid.toString());
        } 
        catch (Exception ex) {
            // ignored
        }
        finally {
            try {
                writer.close();
            } catch (final IOException ignored) {
                // ignored
            }
        }
    }
}
