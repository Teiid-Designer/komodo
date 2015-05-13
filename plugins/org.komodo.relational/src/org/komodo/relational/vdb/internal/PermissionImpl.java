/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data policy permission.
 */
public final class PermissionImpl extends RelationalObjectImpl implements Permission {

    /**
     * The resolver of a {@link Permission}.
     */
    public static final TypeResolver< Permission > RESOLVER = new TypeResolver< Permission >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Permission create( final UnitOfWork transaction,
                                  final Repository repository,
                                  final KomodoObject parent,
                                  final String id,
                                  final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final DataRole parentDataRole = adapter.adapt( transaction, parent, DataRole.class );
            return RelationalModelFactory.createPermission( transaction, repository, parentDataRole, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< PermissionImpl > owningClass() {
            return PermissionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            VdbLexicon.DataRole.Permission.PERMISSION );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Permission resolve( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return new PermissionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

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
        return KomodoType.VDB_PERMISSION;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#addCondition(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Condition addCondition( final UnitOfWork transaction,
                                   final String conditionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( conditionName, "conditionName" ); //$NON-NLS-1$

        final Condition result = RelationalModelFactory.createCondition( transaction, getRepository(), this, conditionName );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#addMask(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Mask addMask( final UnitOfWork transaction,
                         final String maskName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( maskName, "maskName" ); //$NON-NLS-1$

        final Mask result = RelationalModelFactory.createMask( transaction, getRepository(), this, maskName );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Condition[] conditions = getConditions( transaction );
        final Mask[] masks = getMasks( transaction );

        final KomodoObject[] result = new KomodoObject[ conditions.length + masks.length ];
        System.arraycopy( conditions, 0, result, 0, conditions.length );
        System.arraycopy( masks, 0, result, conditions.length, masks.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.DataRole.Permission.Condition.CONDITION.equals( type ) ) {
            result = getConditions( transaction );
        } else if ( VdbLexicon.DataRole.Permission.Mask.MASK.equals( type ) ) {
            result = getMasks( transaction );
        } else {
            result = KomodoObject.EMPTY_ARRAY;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#getConditions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Condition[] getConditions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Condition[] result = null;

        if ( hasChild( transaction, VdbLexicon.DataRole.Permission.CONDITIONS, VdbLexicon.DataRole.Permission.CONDITIONS ) ) {
            final KomodoObject grouping = getChild( transaction,
                                                    VdbLexicon.DataRole.Permission.CONDITIONS,
                                                    VdbLexicon.DataRole.Permission.CONDITIONS );
            final List< Condition > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction,
                                                                           VdbLexicon.DataRole.Permission.Condition.CONDITION ) ) {
                final Condition condition = new ConditionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( condition );
            }

            result = temp.toArray( new Condition[ temp.size() ] );
        } else {
            result = Condition.NO_CONDITIONS;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#getMasks(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Mask[] getMasks( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Mask[] result = null;

        if ( hasChild( transaction, VdbLexicon.DataRole.Permission.MASKS, VdbLexicon.DataRole.Permission.MASKS ) ) {
            final KomodoObject grouping = getChild( transaction,
                                                    VdbLexicon.DataRole.Permission.MASKS,
                                                    VdbLexicon.DataRole.Permission.MASKS );
            final List< Mask > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction, VdbLexicon.DataRole.Permission.Mask.MASK ) ) {
                final Mask mask = new MaskImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( mask );
            }

            result = temp.toArray( new Mask[ temp.size() ] );
        } else {
            result = Mask.NO_MASKS;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( transaction );
        final KomodoObject result = resolveType( transaction, grouping.getParent( transaction ) );
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

        boolean found = false;
        final Condition[] conditions = getConditions( transaction );

        if ( conditions.length != 0 ) {
            for ( final Condition condition : conditions ) {
                if ( conditionToRemove.equals( condition.getName( transaction ) ) ) {
                    condition.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.CONDITION_NOT_FOUND_TO_REMOVE, conditionToRemove ) );
        }
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

        boolean found = false;
        final Mask[] masks = getMasks( transaction );

        if ( masks.length != 0 ) {
            for ( final Mask mask : masks ) {
                if ( maskToRemove.equals( mask.getName( transaction ) ) ) {
                    mask.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.MASK_NOT_FOUND_TO_REMOVE, maskToRemove ) );
        }
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
