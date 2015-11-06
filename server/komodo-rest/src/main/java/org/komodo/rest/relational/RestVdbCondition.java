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
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A condition that can be used by GSON to build a JSON document representation.
 */
public final class RestVdbCondition extends RestBasicEntity {

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

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbCondition() {
        setConstraint(Condition.DEFAULT_CONSTRAINT);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param condition the condition
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbCondition(URI baseUri, Condition condition, UnitOfWork uow) throws KException {
        super(baseUri, condition, uow);

        setName(condition.getName(uow));
        setConstraint(condition.isConstraint(uow));

        Permission permission = ancestor(condition, Permission.class, uow);
        ArgCheck.isNotNull(permission);
        String permName = permission.getName(uow);

        DataRole dataRole = ancestor(permission, DataRole.class, uow);
        ArgCheck.isNotNull(dataRole);
        String dataRoleName = dataRole.getName(uow);

        Vdb vdb = ancestor(dataRole, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        addLink(new RestLink(LinkType.SELF, getUriBuilder()
                             .buildVdbPermissionChildUri(LinkType.SELF, vdbName, dataRoleName, permName, LinkType.MASKS, getId())));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder()
                             .buildVdbPermissionChildUri(LinkType.PARENT, vdbName, dataRoleName, permName, LinkType.MASKS, getId())));
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        tuples.put(NAME_LABEL, newName);
    }

    /**
     * @return the constraint
     */
    public boolean isConstraint() {
        Object constraint = tuples.get(CONSTRAINT_LABEL);
        return constraint != null ? Boolean.parseBoolean(constraint.toString()) : Condition.DEFAULT_CONSTRAINT;
    }

    /**
     * @param constraint the constraint to set
     */
    public void setConstraint(boolean constraint) {
        tuples.put(CONSTRAINT_LABEL, constraint);
    }
}
