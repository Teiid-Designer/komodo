/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A condition that can be used by GSON to build a JSON document representation.
 */
public final class RestVdbCondition extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.Condition.CONDITION);

    /**
     * Label used to describe constraint
     */
    public static final String CONSTRAINT_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);

    /**
     * An empty array of conditions.
     */
    public static final RestVdbCondition[] NO_CONDITIONS = new RestVdbCondition[ 0 ];

    private String name;

    private boolean constraint = Condition.DEFAULT_CONSTRAINT;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbCondition() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param permission the name of the parent permission
     * @param dataRole the name of the parent data role
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbCondition(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren,
                                            String permission, String dataRole, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @return the constraint
     */
    public boolean isConstraint() {
        return this.constraint;
    }

    /**
     * @param constraint the constraint to set
     */
    public void setConstraint(boolean constraint) {
        this.constraint = constraint;
    }

    /**
     * @param condition the source condition
     * @param permission parent permission
     * @param dataRole parent data role
     * @param parentVdb parent vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbCondition} based on the source {@link Permission}
     * @throws KException if error occurs
     */
    public static RestVdbCondition build(Condition condition, Permission permission, DataRole dataRole,
                                                                Vdb parentVdb, URI baseUri,
                                                                UnitOfWork uow) throws KException {
        final String vdbName = parentVdb.getName(uow);
        final String dataRoleName = dataRole.getName(uow);
        final String permissionName = permission.getName(uow);
        final RestVdbCondition entity = new RestVdbCondition(baseUri,
                                                                                       condition.getName(uow),
                                                                                       condition.getAbsolutePath(),
                                                                                       condition.getTypeIdentifier(uow),
                                                                                       condition.hasChildren(uow),
                                                                                       permissionName, dataRoleName, vdbName);

        entity.setName(condition.getName(uow));
        entity.setConstraint(condition.isConstraint(uow));

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.constraint ? 1231 : 1237);
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
        RestVdbCondition other = (RestVdbCondition)obj;
        if (this.constraint != other.constraint)
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
        return "RestVdbCondition [name=" + this.name + ", constraint=" + this.constraint + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
