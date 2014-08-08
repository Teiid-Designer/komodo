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

package org.komodo.modeshape.teiid.sql.lang;

import java.util.Map;
import org.komodo.spi.query.sql.lang.ISourceHint;

/**
 *
 */
public class SourceHint implements ISourceHint {

    /**
     *
     */
    public static class SpecificHint {

        /**
         * @param hint
         * @param useAliases
         */
        public SpecificHint(String hint, boolean useAliases) {
            this.setHint(hint);
            this.setUseAliases(useAliases);
        }

        /**
         * @param hint
         */
        private void setHint(String hint) {
        }

        /**
         * @return hint
         */
        public String getHint() {
            return null;
        }

        /**
         * @return use aliases
         */
        public boolean isUseAliases() {
            return false;
        }

        /**
         * @param useAliases
         */
        private void setUseAliases(boolean useAliases) {
        }
    }

    /**
     * @return general hint
     */
    public String getGeneralHint() {
        return null;
    }

    /**
     * @param generalHint
     */
    public void setGeneralHint(String generalHint) {
    }

    /**
     * @param sourceName
     *
     * @return source hint with name
     */
    public String getSourceHint(String sourceName) {
        return null;
    }

    /**
     * @param translatorName
     * @param hint
     * @param useAliases
     */
    public void setSourceHint(String translatorName, String hint, boolean useAliases) {
    }

    /**
     * @return map of source hints
     */
    public Map<String, SpecificHint> getSourceHints() {
        return null;
    }

    /**
     * @param sourceName
     *
     * @return specific hint for given name
     */
    public SpecificHint getSpecificHint(String sourceName) {
        return null;
    }

    /**
     * @return map of specific hints
     */
    public Map<String, SpecificHint> getSpecificHints() {
        return null;
    }

    /**
     * @return use aliases flag
     */
    public boolean isUseAliases() {
        return false;
    }

    /**
     * @param useAliases
     */
    public void setUseAliases(boolean useAliases) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getGeneralHint() == null) ? 0 : this.getGeneralHint().hashCode());
        result = prime * result + ((this.getSourceHints() == null) ? 0 : this.getSourceHints().hashCode());
        result = prime * result + (this.isUseAliases() ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SourceHint other = (SourceHint)obj;
        if (this.getGeneralHint() == null) {
            if (other.getGeneralHint() != null)
                return false;
        } else if (!this.getGeneralHint().equals(other.getGeneralHint()))
            return false;
        if (this.getSourceHints() == null) {
            if (other.getSourceHints() != null)
                return false;
        } else if (!this.getSourceHints().equals(other.getSourceHints()))
            return false;
        if (this.isUseAliases() != other.isUseAliases())
            return false;
        return true;
    }

}
