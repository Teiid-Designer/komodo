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
package org.komodo.relational.vdb.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data policy permission.
 */
public final class PermissionImpl extends RelationalObjectImpl implements Permission {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Condition.IDENTIFIER, Mask.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public PermissionImpl( final UnitOfWork uow,
                           final Repository repository,
                           final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Permission.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#addCondition(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Condition addCondition( final UnitOfWork transaction,
                                   final String conditionName ) throws KException {
        return RelationalModelFactory.createCondition( transaction, getRepository(), this, conditionName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#addMask(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Mask addMask( final UnitOfWork transaction,
                         final String maskName ) throws KException {
        return RelationalModelFactory.createMask( transaction, getRepository(), this, maskName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        // check conditions
        KomodoObject[] matches = getConditions( transaction, name );

        if ( matches.length != 0 ) {
            return matches[ 0 ];
        }

        matches = getMasks( transaction, name );

        if ( matches.length != 0 ) {
            return matches[ 0 ];
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.Permission.Condition.CONDITION.equals( typeName ) ) {
            final KomodoObject[] conditions = getConditions( transaction, name );

            if ( conditions.length != 0 ) {
                return conditions[ 0 ];
            }
        } else if ( VdbLexicon.DataRole.Permission.Mask.MASK.equals( typeName ) ) {
            final KomodoObject[] masks = getMasks( transaction, name );

            if ( masks.length != 0 ) {
                return masks[ 0 ];
            }
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( ( namePatterns != null ) && ( namePatterns.length == 1 ) ) {
            if ( VdbLexicon.DataRole.Permission.CONDITIONS.equals( namePatterns[ 0 ] ) ) {
                final KomodoObject grouping = getConditionsGroupingNode( transaction );

                if ( grouping == null ) {
                    return KomodoObject.EMPTY_ARRAY;
                }

                return new KomodoObject[] { grouping };
            }

            if ( VdbLexicon.DataRole.Permission.MASKS.equals( namePatterns[ 0 ] ) ) {
                final KomodoObject grouping = getMasksGroupingNode( transaction );

                if ( grouping == null ) {
                    return KomodoObject.EMPTY_ARRAY;
                }

                return new KomodoObject[] { grouping };
            }
        }

        final Condition[] conditions = getConditions( transaction, namePatterns );
        final Mask[] masks = getMasks( transaction, namePatterns );

        final KomodoObject[] result = new KomodoObject[ conditions.length + masks.length ];
        System.arraycopy( conditions, 0, result, 0, conditions.length );
        System.arraycopy( masks, 0, result, conditions.length, masks.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.DataRole.Permission.Condition.CONDITION.equals( type ) ) {
            result = getConditions( transaction, namePatterns );
        } else if ( VdbLexicon.DataRole.Permission.Mask.MASK.equals( type ) ) {
            result = getMasks( transaction, namePatterns );
        } else {
            result = KomodoObject.EMPTY_ARRAY;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#getConditions(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Condition[] getConditions( final UnitOfWork transaction,
                                      final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Condition[] result = null;
        final KomodoObject grouping = getConditionsGroupingNode( transaction );

        if ( grouping != null ) {
            final List< Condition > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.DataRole.Permission.Condition.CONDITION,
                                                                           namePatterns ) ) {
                final Condition condition = new ConditionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( condition );
            }

            result = temp.toArray( new Condition[ temp.size() ] );
        } else {
            result = Condition.NO_CONDITIONS;
        }

        return result;
    }

    private KomodoObject getConditionsGroupingNode( final UnitOfWork transaction ) {
        try {
            if ( hasRawChild( transaction,
                              VdbLexicon.DataRole.Permission.CONDITIONS,
                              VdbLexicon.DataRole.Permission.CONDITIONS ) ) {
                return getRawChildren( transaction, VdbLexicon.DataRole.Permission.CONDITIONS )[ 0 ];
            }
        } catch ( final KException e ) {
            // nothing to do
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#getMasks(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Mask[] getMasks( final UnitOfWork transaction,
                            final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Mask[] result = null;
        final KomodoObject grouping = getMasksGroupingNode( transaction );

        if ( grouping != null ) {
            final List< Mask > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.DataRole.Permission.Mask.MASK,
                                                                           namePatterns ) ) {
                final Mask mask = new MaskImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( mask );
            }

            result = temp.toArray( new Mask[ temp.size() ] );
        } else {
            result = Mask.NO_MASKS;
        }

        return result;
    }

    private KomodoObject getMasksGroupingNode( final UnitOfWork transaction ) {
        try {
            if ( hasRawChild( transaction, VdbLexicon.DataRole.Permission.MASKS, VdbLexicon.DataRole.Permission.MASKS ) ) {
                return getRawChildren( transaction, VdbLexicon.DataRole.Permission.MASKS )[ 0 ];
            }
        } catch ( final KException e ) {
            // nothing to do
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public DataRole getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( transaction );
        final DataRole result = DataRole.RESOLVER.resolve( transaction, grouping.getParent( transaction ) );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#getResourceName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getResourceName( final UnitOfWork transaction ) throws KException {
        return getName(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        return ( ( getConditions( transaction, name ).length != 0 ) || ( getMasks( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.Permission.Condition.CONDITION.equals( typeName ) ) {
            return ( getConditions( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.DataRole.Permission.Mask.MASK.equals( typeName ) ) {
            return ( getMasks( transaction, name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( UnitOfWork transaction ) throws KException {
        if ( super.hasChildren( transaction ) ) {
            return ( ( getConditions( transaction ).length != 0 ) || ( getMasks( transaction ).length != 0 ) );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowAlter(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowAlter( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowAlter", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_ALTER);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowCreate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowCreate( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowCreate", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_CREATE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowDelete(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowDelete( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowDelete", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_DELETE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowExecute(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowExecute( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowExecute", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_EXECUTE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowLanguage(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowLanguage( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowLanguage", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowRead(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowRead( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowRead", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_READ);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#isAllowUpdate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowUpdate( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowUpdate", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.ALLOW_UPDATE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#removeCondition(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeCondition( final UnitOfWork transaction,
                                 final String conditionToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( conditionToRemove, "conditionToRemove" ); //$NON-NLS-1$

        final Condition[] conditions = getConditions( transaction, conditionToRemove );

        if ( conditions.length == 0 ) {
            throw new KException( Messages.getString( Relational.CONDITION_NOT_FOUND_TO_REMOVE, conditionToRemove ) );
        }

        // remove first occurrence
        conditions[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#removeMask(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeMask( final UnitOfWork transaction,
                            final String maskToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( maskToRemove, "maskToRemove" ); //$NON-NLS-1$

        final Mask[] masks = getMasks( transaction, maskToRemove );

        if ( masks.length == 0 ) {
            throw new KException( Messages.getString( Relational.MASK_NOT_FOUND_TO_REMOVE, maskToRemove ) );
        }

        // remove first occurrence
        masks[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowAlter(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowAlter( final UnitOfWork uow,
                               final boolean newAllowAlter ) throws KException {
        setObjectProperty(uow, "setAllowAlter", VdbLexicon.DataRole.Permission.ALLOW_ALTER, newAllowAlter); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowCreate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowCreate( final UnitOfWork uow,
                                final boolean newAllowCreate ) throws KException {
        setObjectProperty(uow, "setAllowCreate", VdbLexicon.DataRole.Permission.ALLOW_CREATE, newAllowCreate); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowDelete(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowDelete( final UnitOfWork uow,
                                final boolean newAllowDelete ) throws KException {
        setObjectProperty(uow, "setAllowDelete", VdbLexicon.DataRole.Permission.ALLOW_DELETE, newAllowDelete); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowExecute(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowExecute( final UnitOfWork uow,
                                 final boolean newAllowExecute ) throws KException {
        setObjectProperty(uow, "setAllowExecute", VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, newAllowExecute); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowLanguage(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowLanguage( final UnitOfWork uow,
                                  final boolean newAllowLanguage ) throws KException {
        setObjectProperty(uow, "setAllowLanguage", VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, newAllowLanguage); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowRead(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowRead( final UnitOfWork uow,
                              final boolean newAllowRead ) throws KException {
        setObjectProperty(uow, "setAllowRead", VdbLexicon.DataRole.Permission.ALLOW_READ, newAllowRead); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#setAllowUpdate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowUpdate( final UnitOfWork uow,
                                final boolean newAllowUpdate ) throws KException {
        setObjectProperty(uow, "setAllowUpdate", VdbLexicon.DataRole.Permission.ALLOW_UPDATE, newAllowUpdate); //$NON-NLS-1$
    }

}
