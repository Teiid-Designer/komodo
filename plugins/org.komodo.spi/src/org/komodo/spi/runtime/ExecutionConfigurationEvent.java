/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.runtime;

import org.komodo.spi.Messages;
import org.komodo.spi.Messages.SPI;


/**
 * The <code>ExecutionConfigurationEvent</code> class is the event that is broadcast from the {@link TeiidInstanceManager instance manager}
 * when a teiid instance or connector is added, removed, or changed, or when a teiid instance is refreshed.
 *
 *
 */
public final class ExecutionConfigurationEvent {

    public static ExecutionConfigurationEvent createAddDataSourceEvent( ITeiidDataSource dataSource ) {
        return new ExecutionConfigurationEvent(EventType.ADD, TargetType.DATA_SOURCE, dataSource);
    }

    public static ExecutionConfigurationEvent createAddTeiidEvent( ITeiidInstance teiidInstance ) {
        return new ExecutionConfigurationEvent(EventType.ADD, TargetType.TINSTANCE, teiidInstance);
    }

    public static ExecutionConfigurationEvent createDeployVDBEvent( String vdbName ) {
        return new ExecutionConfigurationEvent(EventType.ADD, TargetType.VDB, vdbName);
    }

    public static ExecutionConfigurationEvent createRemoveDataSourceEvent( ITeiidDataSource dataSource ) {
        return new ExecutionConfigurationEvent(EventType.REMOVE, TargetType.DATA_SOURCE, dataSource);
    }

    public static ExecutionConfigurationEvent createRemoveTeiidEvent( ITeiidInstance teiidInstance ) {
        return new ExecutionConfigurationEvent(EventType.REMOVE, TargetType.TINSTANCE, teiidInstance);
    }

    public static ExecutionConfigurationEvent createTeiidRefreshEvent( ITeiidInstance teiidInstance ) {
        return new ExecutionConfigurationEvent(EventType.REFRESH, TargetType.TINSTANCE, teiidInstance);
    }

    public static ExecutionConfigurationEvent createTeiidConnectedEvent( ITeiidInstance teiidInstance ) {
        return new ExecutionConfigurationEvent(EventType.CONNECTED, TargetType.TINSTANCE, teiidInstance);
    }

    public static ExecutionConfigurationEvent createSetDefaultTeiidEvent( ITeiidInstance oldDefaultInstance,
                                                                           ITeiidInstance newDefaultInstance ) {
        return new ExecutionConfigurationEvent(EventType.DEFAULT, TargetType.TINSTANCE, oldDefaultInstance, newDefaultInstance);
    }

    public static ExecutionConfigurationEvent createUnDeployVDBEvent( String vdbName ) {
        return new ExecutionConfigurationEvent(EventType.REMOVE, TargetType.VDB, vdbName);
    }

    public static ExecutionConfigurationEvent createUpdateDataSourceEvent( ITeiidDataSource dataSource ) {
        return new ExecutionConfigurationEvent(EventType.UPDATE, TargetType.DATA_SOURCE, dataSource);
    }

    public static ExecutionConfigurationEvent createUpdateTeiidEvent( ITeiidInstance teiidInstance,
                                                                       ITeiidInstance updatedInstance ) {
        return new ExecutionConfigurationEvent(EventType.UPDATE, TargetType.TINSTANCE, teiidInstance, updatedInstance);
    }

    private final EventType eventType;

    private final TargetType targetType;

    private final Object target;
    private final Object updatedTarget;

    private ExecutionConfigurationEvent( EventType eventType,
                                         TargetType targetType,
                                         Object target ) {
        this(eventType, targetType, target, null);
        if (target == null) {
            throw new IllegalArgumentException(Messages.getString(SPI.valueCannotBeNull, "target")); //$NON-NLS-1$
        }
    }

    private ExecutionConfigurationEvent( EventType eventType,
                                         TargetType targetType,
                                         Object target,
                                         Object updatedTarget ) {
        assert (eventType != null);
        assert (targetType != null);

        this.eventType = eventType;
        this.targetType = targetType;
        this.target = target;
        this.updatedTarget = updatedTarget;
    }

    /**
     * Create a refresh event.
     * 
     * @param targetType the target type that was refreshed
     */
    private ExecutionConfigurationEvent( TargetType targetType ) {
        this(EventType.REFRESH, targetType, null, null);
    }

    /**
     * @return the connector involved in the event
     * @throws IllegalStateException if method is called for a teiid instance event
     */
    public ITeiidDataSource getDataSource() {
        if (this.targetType != TargetType.DATA_SOURCE) {
            throw new IllegalStateException(Messages.getString(Messages.SPI.invalidTargetTypeForGetDataSourceMethod, 
                                                           this.targetType,
                                                           TargetType.DATA_SOURCE));
        }

        return (ITeiidDataSource)this.target;
    }

    /**
     * @return the event type (never <code>null</code>)
     */
    public EventType getEventType() {
        return this.eventType;
    }

    /**
     * When changing the default teiid instance, this returns the old default teiid instance.
     * 
     * @return the teiid instance involved in the event (may be <code>null</code>)
     * @throws IllegalStateException if method is called for a connector event
     */
    public ITeiidInstance getTeiidInstance() {
        if (this.targetType != TargetType.TINSTANCE) {
            throw new IllegalStateException(Messages.getString(Messages.SPI.invalidTargetTypeForGetTeiidMethod,
                                                           this.targetType,
                                                           TargetType.TINSTANCE));
        }

        return (ITeiidInstance)this.target;
    }

    /**
     * @return the target type (never <code>null</code>)
     */
    public TargetType getTargetType() {
        return this.targetType;
    }

    /**
     * @return the connector involved in the event
     * @throws IllegalStateException if method is called for a teiid instance event
     */
    public ITeiidTranslator getTranslator() {
        if (this.targetType != TargetType.TRANSLATOR) {
            throw new IllegalStateException(Messages.getString(Messages.SPI.invalidTargetTypeForGetTranslatorMethod,
                                                           this.targetType,
                                                           TargetType.TRANSLATOR));
        }

        return (ITeiidTranslator)this.target;
    }

    /**
     * When changing the default teiid instance, this returns the new default teiid instance.
     * 
     * @return the updated teiid instance involved in the event (may be <code>null</code>)
     * @throws IllegalStateException if method is called for a connector event
     */
    public ITeiidInstance getUpdatedInstance() {
        if (this.targetType != TargetType.TINSTANCE) {
            throw new IllegalStateException(Messages.getString(Messages.SPI.invalidTargetTypeForGetUpdatedTeiidMethod,
                                                           this.targetType,
                                                           TargetType.TINSTANCE));
        }

        return (ITeiidInstance)this.updatedTarget;
    }

    public enum EventType {
        ADD,
        CONNECTED,
        REFRESH,
        REMOVE,
        UPDATE,
        DEFAULT;
    }

    public enum TargetType {
        TRANSLATOR,
        DATA_SOURCE,
        TINSTANCE,
        VDB,
        SOURCE_BINDING;
    }

}
