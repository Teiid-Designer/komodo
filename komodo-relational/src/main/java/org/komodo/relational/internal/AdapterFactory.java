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
package org.komodo.relational.internal;

import org.komodo.relational.TypeResolver;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * Factory dedicated to adapting a {@link KomodoObject} into its
 * relational object instance.
 */
public class AdapterFactory {

    /**
     */
    public AdapterFactory() {
    }

    /**
     * Attempts to adapt the given object to a relational model typed class.
     * If the object is not an instance of {@link KomodoObject} then null is
     * returned.
     *
     * The type id of the {@link KomodoObject} is extracted and the correct
     * relational model object created. If the latter is not assignable from the
     * given adapted class then it is concluded the adaption should fail and
     * null is returned, otherwise the new object is returned.
     * @param <T> the result's type
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param object object to adapt
     * @param adaptedClass the expected class that the object should be adapted to
     * @return the adapted instance or null
     */
    @SuppressWarnings( "unchecked" )
    public <T> T adapt(UnitOfWork transaction, Object object, Class<T> adaptedClass) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (! (object instanceof KomodoObject))
            return null;

        if (adaptedClass.isInstance(object))
            return (T) object;

        KomodoObject result = null;

        try {
            KomodoObject kObject = (KomodoObject) object;
            KomodoType type = kObject.getTypeIdentifier(transaction);
            TypeResolverRegistry registry = TypeResolverRegistry.getInstance();
            TypeResolver< ? > resolver = registry.getResolver(type);

            if (resolver != null && resolver.resolvable(transaction, kObject))
                result = resolver.resolve(transaction, kObject);

            if (result == null) {
                // Failed with the type identifier so try to be safe than sorry
                // and iterate through all resolvers to check this object is really
                // not resolvable.
                for (final TypeResolver< ? > aResolver : registry.getResolvers()) {
                    if (aResolver.resolvable(transaction, kObject)) {
                        result = aResolver.resolve(transaction, kObject);
                        break;
                    }
                }
            }

            if (result == null)
                return null; // Type cannot be resolved so cannot be adapted

        } catch (final Exception e) {
            // No need to log error
        }

        if (result == null)
            return null;

        if (! adaptedClass.isAssignableFrom(result.getClass()))
            return null;

        return (T) result;
    }

}
