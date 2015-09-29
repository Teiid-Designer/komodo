/*
 * Copyright 2012 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell.api;

import java.util.Map;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;


/**
 * The ShellCommandProvider interface
 */
public interface ShellCommandProvider {

	/**
	 * Called to get the collection of commands contributed by the provider.
	 * @return the map of commands
	 */
	public Map<String, Class<? extends ShellCommand>> provideCommands();

    /**
     * Resolve the supplied KomodoObject
     * @param <T> the specific {@link KomodoObject} type being resolved
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return resolved object
     * @throws KException the exception
     */
    public < T extends KomodoObject > T resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Determine if the supplied KomodoObject is the context root
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return 'true' if the KomodoObject is the root object
     * @throws KException the exception
     */
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Get the type display string for a KomodoObject
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return the type display string
     * @throws KException the exception
     */
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Initialize workspace state for recognized properties
     * @param wsStatus the workspace status
     * @throws KException the exception
     */
    public void initWorkspaceState ( final WorkspaceStatus wsStatus ) throws KException;

    /**
     * Get status message for the provided KomodoObject
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return the status message
     * @throws KException the exception
     */
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException;

}
