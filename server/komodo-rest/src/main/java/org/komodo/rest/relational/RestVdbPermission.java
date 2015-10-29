/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A data role that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyPermission",
 *     "allowAlter" : true,
 *     "allowCreate" : true,
 *     "allowDelete" : true,
 *     "allowExecute" : true,
 *     "allowLanguage" : true,
 *     "allowRead" : true,
 *     "allowUpdate" : true,
 *     "conditions" : {
 *         "cant" : true,
 *         "buy" : false,
 *         "me" : true,
 *         "love" : false
 *     },
 *     "masks" : {
 *         "love" : "words",
 *         "me" : "of",
 *         "do" : "love"
 *     }
 * }
 * </code>
 * </pre>
 */
public final class RestVdbPermission extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.PERMISSION);

    /**
     * Label used to describe allowAlter
     */
    public static final String ALLOW_ALTER_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_ALTER);

    /**
     * Label used to describe allowCreate
     */
    public static final String ALLOW_CREATE_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_CREATE);

    /**
     * Label used to describe allowDelete
     */
    public static final String ALLOW_DELETE_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_DELETE);

    /**
     * Label used to describe allowExecute
     */
    public static final String ALLOW_EXECUTE_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_EXECUTE);

    /**
     * Label used to describe allowLanguage
     */
    public static final String ALLOW_LANGUAGE_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE);

    /**
     * Label used to describe allowRead
     */
    public static final String ALLOW_READ_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_READ);

    /**
     * Label used to describe allowUpdate
     */
    public static final String ALLOW_UPDATE_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.ALLOW_UPDATE);

    /**
     * Label used to describe conditions
     */
    public static final String CONDITIONS_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.CONDITIONS);

    /**
     * Label used to describe masks
     */
    public static final String MASKS_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.MASKS);

    /**
     * An empty array of permissions.
     */
    public static final RestVdbPermission[] NO_PERMISSIONS = new RestVdbPermission[ 0 ];

    private String name;
    private boolean allowAlter = Permission.DEFAULT_ALLOW_ALTER;
    private boolean allowCreate = Permission.DEFAULT_ALLOW_CREATE;
    private boolean allowDelete = Permission.DEFAULT_ALLOW_DELETE;
    private boolean allowExecute = Permission.DEFAULT_ALLOW_EXECUTE;
    private boolean allowLanguage = Permission.DEFAULT_ALLOW_LANGUAGE;
    private boolean allowRead = Permission.DEFAULT_ALLOW_READ;
    private boolean allowUpdate = Permission.DEFAULT_ALLOW_UPDATE;
    private RestVdbCondition[] conditions = RestVdbCondition.NO_CONDITIONS;
    private RestVdbMask[] masks = RestVdbMask.NO_MASKS;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbPermission() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param dataRole the name of the parent data role
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbPermission(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren,
                                            String dataRole, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }
    /**
     * @return <code>true</code> if allows alter
     */
    public boolean isAllowAlter() {
        return this.allowAlter;
    }

    /**
     * @return <code>true</code> if allows create
     */
    public boolean isAllowCreate() {
        return this.allowCreate;
    }

    /**
     * @return <code>true</code> if allows delete
     */
    public boolean isAllowDelete() {
        return this.allowDelete;
    }

    /**
     * @return <code>true</code> if allows execute
     */
    public boolean isAllowExecute() {
        return this.allowExecute;
    }

    /**
     * @return <code>true</code> if allows language
     */
    public boolean isAllowLanguage() {
        return this.allowLanguage;
    }

    /**
     * @return <code>true</code> if allows read
     */
    public boolean isAllowRead() {
        return this.allowRead;
    }

    /**
     * @return <code>true</code> if allows update
     */
    public boolean isAllowUpdate() {
        return this.allowUpdate;
    }

    /**
     * @param newAllowAlter
     *        <code>true</code> if allows alter
     */
    public void setAllowAlter( final boolean newAllowAlter ) {
        this.allowAlter = newAllowAlter;
    }

    /**
     * @param newAllowCreate
     *        <code>true</code> if allows create
     */
    public void setAllowCreate( final boolean newAllowCreate ) {
        this.allowCreate = newAllowCreate;
    }

    /**
     * @param newAllowDelete
     *        <code>true</code> if allows delete
     */
    public void setAllowDelete( final boolean newAllowDelete ) {
        this.allowDelete = newAllowDelete;
    }

    /**
     * @param newAllowExecute
     *        <code>true</code> if allows execute
     */
    public void setAllowExecute( final boolean newAllowExecute ) {
        this.allowExecute = newAllowExecute;
    }

    /**
     * @param newAllowLanguage
     *        <code>true</code> if allows language
     */
    public void setAllowLanguage( final boolean newAllowLanguage ) {
        this.allowLanguage = newAllowLanguage;
    }

    /**
     * @param newAllowRead
     *        <code>true</code> if allows read
     */
    public void setAllowRead( final boolean newAllowRead ) {
        this.allowRead = newAllowRead;
    }

    /**
     * @param newAllowUpdate
     *        <code>true</code> if allows update
     */
    public void setAllowUpdate( final boolean newAllowUpdate ) {
        this.allowUpdate = newAllowUpdate;
    }

    /**
     * @return the conditions (never <code>null</code> but can be empty)
     */
    public RestVdbCondition[] getConditions() {
        return this.conditions;
    }

    /**
     * @param newConditions
     *        the new conditions (can be <code>null</code> or empty)
     */
    public void setConditions( final RestVdbCondition[] newConditions ) {
        this.conditions = ( ( newConditions == null ) ? RestVdbCondition.NO_CONDITIONS : newConditions );
    }

    /**
     * @return the masks (never <code>null</code> but can be empty)
     */
    public RestVdbMask[] getMasks() {
        return this.masks;
    }

    /**
     * @param newMasks
     *        the new masks (can be <code>null</code> or empty)
     */
    public void setMasks( final RestVdbMask[] newMasks ) {
        this.masks = ( ( newMasks == null ) ? RestVdbMask.NO_MASKS : newMasks );
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param permission parent permission
     * @param dataRole parent data role
     * @param parentVdb parent vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbCondition} based on the source {@link Permission}
     * @throws KException if error occurs
     */
    public static RestVdbPermission build(Permission permission, DataRole dataRole,
                                                                  Vdb parentVdb, URI baseUri, UnitOfWork uow) throws KException {
        final String vdbName = parentVdb.getName(uow);
        final String dataRoleName = dataRole.getName(uow);
        final RestVdbPermission entity = new RestVdbPermission(baseUri,
                                                                                       permission.getName(uow),
                                                                                       permission.getAbsolutePath(),
                                                                                       permission.getTypeIdentifier(uow),
                                                                                       permission.hasChildren(uow),
                                                                                       dataRoleName, vdbName);

        entity.setName(permission.getName(uow));
        entity.setAllowAlter(permission.isAllowAlter(uow));
        entity.setAllowCreate(permission.isAllowCreate(uow));
        entity.setAllowDelete(permission.isAllowDelete(uow));
        entity.setAllowExecute(permission.isAllowExecute(uow));
        entity.setAllowLanguage(permission.isAllowLanguage(uow));
        entity.setAllowRead(permission.isAllowRead(uow));
        entity.setAllowUpdate(permission.isAllowUpdate(uow));

        Condition[] conditions = permission.getConditions(uow);
        if (conditions != null) {
            List<RestVdbCondition> restConditions = new ArrayList<>();
            for (Condition condition : conditions) {
                RestVdbCondition restCondition = RestVdbCondition.build(condition, permission, dataRole, parentVdb, baseUri, uow);
                restConditions.add(restCondition);
            }

            entity.setConditions(restConditions.toArray(new RestVdbCondition[0]));
        }

        Mask[] masks = permission.getMasks(uow);
        if (masks != null) {
            List<RestVdbMask> restMasks = new ArrayList<>();
            for (Mask mask : masks) {
                RestVdbMask restMask = RestVdbMask.build(mask, permission, dataRole, parentVdb, baseUri, uow);
                restMasks.add(restMask);
            }

            entity.setMasks(restMasks.toArray(new RestVdbMask[0]));
        }

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.allowAlter ? 1231 : 1237);
        result = prime * result + (this.allowCreate ? 1231 : 1237);
        result = prime * result + (this.allowDelete ? 1231 : 1237);
        result = prime * result + (this.allowExecute ? 1231 : 1237);
        result = prime * result + (this.allowLanguage ? 1231 : 1237);
        result = prime * result + (this.allowRead ? 1231 : 1237);
        result = prime * result + (this.allowUpdate ? 1231 : 1237);
        result = prime * result + ((this.conditions == null) ? 0 : this.conditions.hashCode());
        result = prime * result + ((this.masks == null) ? 0 : this.masks.hashCode());
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestVdbPermission other = (RestVdbPermission)obj;
        if (this.allowAlter != other.allowAlter)
            return false;
        if (this.allowCreate != other.allowCreate)
            return false;
        if (this.allowDelete != other.allowDelete)
            return false;
        if (this.allowExecute != other.allowExecute)
            return false;
        if (this.allowLanguage != other.allowLanguage)
            return false;
        if (this.allowRead != other.allowRead)
            return false;
        if (this.allowUpdate != other.allowUpdate)
            return false;
        if (this.conditions == null) {
            if (other.conditions != null)
                return false;
        } else
            if (!this.conditions.equals(other.conditions))
                return false;
        if (this.masks == null) {
            if (other.masks != null)
                return false;
        } else
            if (!this.masks.equals(other.masks))
                return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbPermission [name=" + this.name + ", allowAlter=" + this.allowAlter + ", allowCreate=" + this.allowCreate + ", allowDelete=" + this.allowDelete + ", allowExecute=" + this.allowExecute + ", allowLanguage=" + this.allowLanguage + ", allowRead=" + this.allowRead + ", allowUpdate=" + this.allowUpdate + ", conditions=" + this.conditions + ", masks=" + this.masks + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
